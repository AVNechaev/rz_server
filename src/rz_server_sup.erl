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

  StockOpenF = fun() -> iql1_conn:get_stock_open_utc() end,
  FrameModule = iqfeed_util:get_env(rz_server, timeframe_module),
  Frames = [
    {
      FrameModule:reg_name(Name),
      {FrameModule, start_link, [Name, [{stock_open_fun, StockOpenF} | Params]]},
      permanent, brutal_kill, worker, [FrameModule]
    } || {Name, Params} <- iqfeed_util:get_env(rz_server, frames)
  ],

  Cache = {candles_cache,
    {candles_cached_store, start_link, [
      iqfeed_util:get_env(rz_server, cache_tables),
      iqfeed_util:get_env(rz_server, cache_size),
      iqfeed_util:get_env(rz_server, cache_timeout)
    ]},
    permanent, brutal_kill, worker, [candles_cached_store]},

  FiresCache = {fires_cache,
    {fires_cached_store, start_link, [
      iqfeed_util:get_env(rz_server, fires_cache_size),
      iqfeed_util:get_env(rz_server, fires_cache_timeout)
    ]},
    permanent, brutal_kill, worker, [candles_cached_store]},

  PatternsExecutor = {patterns_executor,
    {patterns_executor, start_link, []},
    permanent, brutal_kill, worker, [patterns_executor]},

  PatternsStore = {pattern_store,
    {pattern_store, start_link, []},
    permanent, brutal_kill, worker, [pattern_store]},

  IQFeed = {iqfeed,
    {iq_sup, start_link, [TickFun]},
    permanent, infinity, supervisor, [iq_sup]},

  Web = {webmachine,
    {webmachine_mochiweb, start, [[
      {ip, "127.0.0.1"},
      {port, iqfeed_util:get_env(rz_server, http_port)},
      {backlog, 1000},
      {dispatch, [
        {["nyse", "instrs"], web_instruments, []}, %% POST, PUT, GET
        {["nyse", "instrs", instr], web_single_instrument, []}, %% DELETE
        {["patterns"], web_patterns, []}, %% POST, GET
        {["patterns", pattern_id], web_single_pattern, []} %% DELETE
      ]}
    ]]},
    permanent, brutal_kill, worker, dynamic},

  {
    ok,
    {{one_for_one, 5, 10},
      lists:flatten([
        MemCached,
        Hist,
        Frames,
        Cache,
        FiresCache,
        PatternsExecutor,
        PatternsStore,
        IQFeed,
        Web
      ])}}.

