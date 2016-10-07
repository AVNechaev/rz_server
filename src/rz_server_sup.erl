-module(rz_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% при запросе истории для заполнения SMA могут быть пропуски
%% по датам из-за выходных и каникул; поэтому берем существенно
%% большую глубину поиска, а ненужные старые данные будут
%% вытеснены из очереди внутри SMA_STORE
-define(DEPTH_WITH_VACATIONS(Depth), trunc(Depth * 1.5)).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").
-compile([{parse_transform, lager_transform}]).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ok = emysql:add_pool(mysql_candles_store, rz_util:get_env(rz_server, mysql_candles_store)),
  ok = emysql:add_pool(mysql_config_store, rz_util:get_env(rz_server, mysql_config_store)),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Names = [timeframe_worker:reg_name(N) || {N, _} <- rz_util:get_env(rz_server, frames)],
  TickFun =
    fun(Tick) ->
      lists:foreach(fun(N) -> timeframe_worker:add_tick(N, Tick) end, Names)
    end,

  {ok, Instr} = rz_util:load_instr_csv(
    rz_util:get_env(iqfeed_client, instr_file),
    rz_util:get_env(iqfeed_client, instr_file_header),
    rz_util:get_env(iqfeed_client, instr_defaults)
  ),

  MemCached = {memcached,
    {erlmc, start_link, [[{
      rz_util:get_env(rz_server, memcached_ip),
      rz_util:get_env(rz_server, memcached_port),
      1}]
    ]},
    permanent, brutal_kill, worker, [erlmc]
  },
  Hist = [
    {
      online_history_worker:reg_name(Name),
      {online_history_worker, start_link, [Name, Params, Instr]},
      permanent, brutal_kill, worker, [online_history_worker]
    } || {Name, Params} <- rz_util:get_env(rz_server, frames)
  ],

  StockOpenF = fun() -> iql1_conn:get_stock_open_utc() end,
  FrameModule = rz_util:get_env(rz_server, timeframe_module),
  Frames = [
    {
      FrameModule:reg_name(Name),
      {FrameModule, start_link, [Name, [{stock_open_fun, StockOpenF} | Params], Instr]},
      permanent, brutal_kill, worker, [FrameModule]
    } || {Name, Params} <- rz_util:get_env(rz_server, frames)
  ],

  Cache = {candles_cache,
    {candles_cached_store, start_link, []},
    permanent, brutal_kill, worker, [candles_cached_store]},

  FiresCache = {fires_cache,
    {fires_cached_store, start_link, [
      rz_util:get_env(rz_server, fires_cache_size),
      rz_util:get_env(rz_server, fires_cache_timeout)
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
      {port, rz_util:get_env(rz_server, http_port)},
      {backlog, 1000},
      {dispatch, [
        {["nyse", "instrs"], web_instruments, []}, %%PUT
        {["patterns"], web_patterns, []}, %% POST, GET
        {["patterns", pattern_id], web_single_pattern, []} %% PUT, DELETE
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

