%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Апр. 2016 18:10
%%%-------------------------------------------------------------------
-module(tf_worker_v2).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/2, reg_name/1, add_tick/2, storage_name/1, get_current_candle/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").
-include("internal.hrl").

-record(candle_params, {
  name :: instr_name(),
%%   start :: pos_integer() | undefined, %% время начала свечи
  tref = undefined :: reference() | undefined, %% таймер, срабатывающий по протуханию свечи
  last_flushed :: pos_integer() | undefined %% время последнего флуша свечи
}).

-record(state, {
  trading_start :: calendar:time(),
  data_tid :: ets:tid(),
  params_tid :: ets:tid(),
  expired_ticks = 0 :: non_neg_integer(), %% количество устаревших тиков (пришедших по времени после флуша соотв свечки)
  duration :: pos_integer(), %длительность свечки
  name :: atom(),
  name_bin :: binary(),
  history_name :: atom(),
  reinit_timeout :: non_neg_integer()
}).

-define(MAX_SKIP_EXPIRED_TICKS_BEFORE_LOG, 1000).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(Name :: atom(), Params :: frame_params()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Params) ->
  gen_server:start_link({local, reg_name(Name)}, ?MODULE, [Name, Params], []).

%%--------------------------------------------------------------------
-spec reg_name(Name :: atom()) -> atom().
reg_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_frame_wroker").

%%--------------------------------------------------------------------
-spec add_tick(ThisName :: atom(), Tick :: #tick{}) -> ok.
add_tick(ThisName, Tick) -> gen_server:cast(ThisName, {add_tick, Tick}).

%%--------------------------------------------------------------------
-spec storage_name(FrameName :: atom()) -> atom().
storage_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_ets_current_candles").

%%--------------------------------------------------------------------
-spec get_current_candle(InstrName :: atom(), StorageName :: atom()) -> {ok, #candle{}} | {error, not_found}.
get_current_candle(InstrName, StorageName) ->
  case ets:lookup(StorageName, InstrName) of
    [] -> {error, not_found};
    [C] -> {ok, C}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Params]) ->
  CandleDataTid = ets:new(storage_name(Name), [named_table, protected, set, {keypos, #candle.name}]),
  CandleParamsTid = ets:new(local, [private, set, {keypos, #candle_params.name}]),
  {ok,
    #state{
      data_tid = CandleDataTid,
      params_tid = CandleParamsTid,
      trading_start = iqfeed_util:get_env(rz_server, trading_start),
      duration = proplists:get_value(duration, Params),
      name = Name,
      name_bin = atom_to_binary(Name, latin1),
      history_name = online_history_worker:reg_name(Name),
      reinit_timeout = proplists:get_value(reinit_timeout, Params)
    }}.

%%--------------------------------------------------------------------
handle_cast({add_tick, Tick}, State) ->
  case is_tick_expired(Tick, State) of
    false ->
      update_candle(Tick, State),
      {noreply, State};
    true ->
      {noreply, log_expired_ticks(State#state{expired_ticks = State#state.expired_ticks + 1}, ?MAX_SKIP_EXPIRED_TICKS_BEFORE_LOG)}
  end.

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
is_tick_expired(#tick{name = Name, time = T}, #state{params_tid = Tid}) ->
  case ets:lookup(Tid, Name) of
    [] -> false;
    [#candle_params{last_flushed = LF}] when T >= LF -> false;
    _ -> true
  end.

%%--------------------------------------------------------------------
update_candle(#tick{name = Name, last_price = LP, last_vol = LV, bid = Bid, ask = Ask, time = Time}, State = #state{data_tid = DTid}) ->
%%   если свеча есть:
%%    если пора флушить, то: флушить, проинитить, сохранить
%%    если не пора, то сохранить
%%   если нет, то проинитить, сохранить
  case ets:lookup(DTid, Name) of
    [{}] -> ok;
    [] ->
      CandleStart = calc_candle_start(Time, State),
      NewCandle = #candle{
        name = Name,
        open = LP,
        close = LP,
        high = LP,
        low = LP,
        vol = LV,
        start = CandleStart,
        start_mysql = util:datetime_to_mysql(CandleStart)
      },
      candle_to_memcached(NewCandle, State#state.name_bin),
      true = ets:insert_new(DTid, NewCandle);
  end.

%%--------------------------------------------------------------------
log_expired_ticks(State = #state{expired_ticks = T}, Limit) when T > Limit ->
  lager:warning("Catch EXPIRED ticks: ~p", [T]),
  State#state{expired_ticks = 0};
log_expired_ticks(State, _) -> State.

%%--------------------------------------------------------------------
calc_candle_start(TickTime, #state{duration = Duration, trading_start = TradingStart}) ->
  {D, _} = calendar:gregorian_seconds_to_datetime(TickTime),
  ExpectedStart = (TickTime div Duration) * Duration,
  TradingStartSeconds = calendar:datetime_to_gregorian_seconds({D, TradingStart}),
  if
    ExpectedStart < TradingStartSeconds -> TradingStartSeconds;
    true -> ExpectedStart
  end.