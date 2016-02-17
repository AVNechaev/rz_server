-module(rz_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ok = emysql:add_pool(mysql, iqfeed_util:get_env(rz_server, emysql)),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Names = [timeframe_worker:reg_name(N) || {N, _} <- iqfeed_util:get_env(rz_server, frames)],
  TickFun = fun(Tick) ->
    lists:foreach(fun(N) -> timeframe_worker:add_tick(N, Tick) end, Names)
    end,

  {ok, Instr} = iqfeed_util:load_instr_csv(
    iqfeed_util:get_env(iqfeed_client, instr_file),
    iqfeed_util:get_env(iqfeed_client, instr_file_header)
  ),

  Hist = [
    {
      online_history_worker:reg_name(Name),
      {online_history_worker, start_link, [Name, Params, Instr]},
      permanent, brutal_kill, worker, [online_history_worker]
    } || {Name, Params} <- iqfeed_util:get_env(rz_server, frames)
  ],

  Frames = [
    {
      timeframe_worker:reg_name(Name),
      {timeframe_worker, start_link, [Name, Params]},
      permanent, brutal_kill, worker, [timeframe_worker]
    } || {Name, Params} <- iqfeed_util:get_env(rz_server, frames)
  ],

  Cache = {candles_cache,
    {candles_cached_store, start_link, [
      iqfeed_util:get_env(rz_server, cache_tables),
      iqfeed_util:get_env(rz_server, cache_size),
      iqfeed_util:get_env(rz_server, cache_timeout)
    ]},
    permanent, brutal_kill, worker, [candles_cached_store]},

  IQFeed = {iqfeed,
    {iq_sup, start_link, [TickFun]},
    permanent, infinity, supervisor, [iq_sup]},

  {
    ok,
    {{one_for_one, 5, 10},
      lists:flatten([
        Hist,
        Frames,
        Cache,
        IQFeed
      ])}}.

