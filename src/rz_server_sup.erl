-module(rz_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").
-compile([{parse_transform, lager_transform}]).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ok = emysql:add_pool(mysql_candles_store, iqfeed_util:get_env(rz_server, mysql_candles_store)),
  ok = emysql:add_pool(mysql_config_store, iqfeed_util:get_env(rz_server, mysql_config_store)),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Names = [timeframe_worker:reg_name(N) || {N, _} <- iqfeed_util:get_env(rz_server, frames)],
  TickFun = fun(Tick) ->
    lager:info("TICK:~p", [Tick]),
    lists:foreach(fun(N) -> timeframe_worker:add_tick(N, Tick) end, Names),
    patterns_executor:check_patterns(Tick#tick.name)
    end,

  {ok, Instr} = iqfeed_util:load_instr_csv(
    iqfeed_util:get_env(iqfeed_client, instr_file),
    iqfeed_util:get_env(iqfeed_client, instr_file_header)
  ),

  MemCached = {memcached,
    {erlmc, start_link, [[{
      iqfeed_util:get_env(rz_server, memcached_ip),
      iqfeed_util:get_env(rz_server, memcached_port),
      1}]
    ]},
    permanent, brutal_kill, worker, [erlmc]
  },
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

  PatternsExecutor = {patterns_executor,
    {patterns_executor, start_link, []},
    permanent, brutal_kill, worker, [patterns_executor]},

  IQFeed = {iqfeed,
    {iq_sup, start_link, [TickFun]},
    permanent, infinity, supervisor, [iq_sup]},

  {
    ok,
    {{one_for_one, 5, 10},
      lists:flatten([
        MemCached,
        Hist,
        Frames,
        Cache,
        PatternsExecutor,
        IQFeed
      ])}}.

