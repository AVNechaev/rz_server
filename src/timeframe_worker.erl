%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Февр. 2016 19:18
%%%-------------------------------------------------------------------
-module(timeframe_worker).
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

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

-type fires_fun() :: fun((InstrName :: instr_name, TickCandleTime :: pos_integer()) -> ok).

-include("internal.hrl").

-record(state, {
  stock_open_f :: stock_open_fun(),
  fires_fun :: fires_fun(),
  empty :: boolean(),
  candles_start :: pos_integer() | undefined,
  candles_start_utc :: pos_integer() | undefined,
  candles_start_bin :: binary() | undefined,
  candles_last_flushed :: pos_integer() | undefined,
  expired_ticks = 0 :: non_neg_integer(), %% количество устаревших тиков (пришедших по времени после флуша соотв свечки)
  tid :: ets:tid(),
  sma_tid :: ets:tid(),
  current_tref = undefined :: undefined | reference(),
  duration :: pos_integer(), %длительность свечки
  name :: atom(),
  name_bin :: binary(),
  history_name :: atom(),
  reinit_timeout :: non_neg_integer(),
  epoch_start :: non_neg_integer(), %% время в секундах 1.01.1970
  known_smas :: [atom()], %% список SMA, по которым есть данные в ETS sma_tid
  cache_context :: term()
}).

-type frame_params() :: list().

-define(MAX_SKIP_EXPIRED_TICKS_BEFORE_LOG, 1000).
-define(LOG_ALL_EXPIRED_TICKS, 0).

-define(SMA_QUEUE_KEY(Instr, SMAName), {Instr, SMAName}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(Name :: atom(), Params :: frame_params()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Params) ->
  gen_server:start_link({local, reg_name(Name)}, ?MODULE, [Name, Params], []).

%%--------------------------------------------------------------------
-spec reg_name(Name :: atom()) -> atom().
reg_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_frame_worker").

%%--------------------------------------------------------------------
-spec add_tick(ThisName :: atom(), Tick :: #tick{}) -> ok.
add_tick(ThisName, Tick) -> gen_server:cast(ThisName, {add_tick, Tick}).

%%--------------------------------------------------------------------
-spec storage_name(FrameName :: atom()) -> atom().
storage_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_ets_current_candles").

%%--------------------------------------------------------------------
-spec get_current_candle(InstrName :: instr_name(), StorageName :: atom()) -> {ok, #candle{}} | {error, not_found}.
get_current_candle(InstrName, StorageName) ->
  case ets:lookup(StorageName, InstrName) of
    [] -> {error, not_found};
    [C] -> {ok, C}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Params]) ->
  Tid = ets:new(storage_name(Name), [named_table, protected, set, {keypos, #candle.name}]),
  SMATid = ets:new(sma_storage_name(Name), [private, set]),

  FiresFun =
    case proplists:get_value(fires_data, Params, false) of
      true -> fun active_fires_fun/2;
      false -> fun inactive_fires_fun/2
    end,
  {ok,
    #state{
      stock_open_f = proplists:get_value(stock_open_fun, Params),
      fires_fun = FiresFun,
      empty = true,
      tid = Tid,
      sma_tid = SMATid,
      duration = proplists:get_value(duration, Params),
      name = Name,
      name_bin = atom_to_binary(Name, latin1),
      history_name = online_history_worker:reg_name(Name),
      reinit_timeout = proplists:get_value(reinit_timeout, Params),
      epoch_start = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
      known_smas = [{Name, FieldName} || {Name, FieldName, _Depth} <- rz_util:get_env(rz_server, sma)],
      cache_context = candles_cached_store:init_context(Name)
    }}.

%%--------------------------------------------------------------------
handle_cast({add_tick, Tick}, State = #state{empty = true, candles_last_flushed = LastFlushed}) ->
  if
    LastFlushed == undefined orelse Tick#tick.time > LastFlushed ->
      NewState = reinit_state(Tick#tick.time, State),
      Candle = update_current_candle(Tick, NewState),
      (NewState#state.fires_fun)(Candle, universal_to_candle_time(NewState)),
      {noreply, NewState#state{empty = false}};
    true ->
      {noreply, log_expired_ticks(State#state{expired_ticks = State#state.expired_ticks + 1}, ?MAX_SKIP_EXPIRED_TICKS_BEFORE_LOG)}
  end;
%%---
handle_cast({add_tick, Tick}, State = #state{candles_last_flushed = LastFlushed}) ->
  if
    LastFlushed == undefined orelse Tick#tick.time > LastFlushed ->
      NewState = if
                   (Tick#tick.time - State#state.candles_start) >= State#state.duration ->
                     reinit_state(Tick#tick.time, State);
                   true -> State
                 end,
      Candle = update_current_candle(Tick, NewState),
      (NewState#state.fires_fun)(Candle, universal_to_candle_time(NewState)),
      {noreply, NewState};
    true ->
      {noreply, log_expired_ticks(State#state{expired_ticks = State#state.expired_ticks + 1}, ?MAX_SKIP_EXPIRED_TICKS_BEFORE_LOG)}
  end.

%%--------------------------------------------------------------------
handle_info({timeout, _, reinit}, State = #state{empty = true}) -> {noreply, State};
%% этот вариант возможен, когда в 1 секунду сначала придет отсчет, пересчитается новый таймер, а потом сработает старый
handle_info({timeout, TRef, reinit}, State = #state{current_tref = OtherTRef}) when TRef =/= OtherTRef ->
  {noreply, State};
handle_info({timeout, _, reinit}, State) ->
  flush_candles(State),
  ets:delete_all_objects(State#state.tid),
  LastFlushed = State#state.candles_start + State#state.duration, %% по идее не должно быть UNDEFINED, т.к. таймер взводится в reinit, но на всякий случай
  {noreply, log_expired_ticks(State#state{empty = true, candles_last_flushed = LastFlushed}, ?LOG_ALL_EXPIRED_TICKS)}.

%%--------------------------------to_date------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_current_candle(Tick = #tick{name = Name, last_price = LP, last_vol = LV, bid = Bid, ask = Ask}, State = #state{tid = Tid}) ->
  SMAValues = update_sma_queues(Tick, State#state.sma_tid, State#state.known_smas),
  case ets:lookup(Tid, Name) of
    [] ->
      NewCandle = #candle{name = Name, open = LP, close = LP, high = LP, low = LP, vol = LV, smas = SMAValues},
      candle_to_memcached(NewCandle, State),
      true = ets:insert_new(Tid, NewCandle),
      NewCandle;
    [C] ->
      U1 = [
        {#candle.close, LP},
        {#candle.vol, C#candle.vol + LV},
        {#candle.bid, Bid},
        {#candle.ask, Ask},
        {#candle.smas, SMAValues}],

      U2 = if
             LP > C#candle.high -> [{#candle.high, LP} | U1];
             true -> U1
           end,
      U3 = if
             LP < C#candle.low -> [{#candle.low, LP} | U2];
             true -> U2
           end,
      NewCandle = #candle{
        name = C#candle.name,
        open = C#candle.open,
        close = LP,
        vol = C#candle.vol + LV,
        bid = Bid,
        ask = Ask,
        high = proplists:get_value(#candle.high, U3, C#candle.high),
        low = proplists:get_value(#candle.low, U3, C#candle.low)
      },
      candle_to_memcached(NewCandle, State),
      ets:update_element(Tid, C#candle.name, U3),
      NewCandle
  end.

%%--------------------------------------------------------------------
reinit_state(TickTime, State = #state{duration = Duration, reinit_timeout = RenitTO}) ->
  case State#state.empty of
    false -> flush_candles(State);
    true -> ok
  end,
  ets:delete_all_objects(State#state.tid),
  ExpectedStart = (TickTime div Duration) * Duration,
  StockOpen = (State#state.stock_open_f)(),
  Start = if
            ExpectedStart < StockOpen -> StockOpen;
            true -> ExpectedStart
          end,
  % если на момент срабатывания таймера есть еще тики в очереди, то надо сначала
  % обработать их; иначе при флуде тиков происходит повторная инициализация свечки
  % как вариант, можно при достаточной пропускной способности поставить
  % в reinit_timer (duration + ReinitTimeout), думая, что за N секунд разгребутся остатки
  % тиков предыдущей секунды
  TRef = erlang:start_timer((Duration + RenitTO) * 1000, self(), reinit),
  lager:info("REINIT CANDLE DURATON ~p AT ~p", [State#state.duration, calendar:gregorian_seconds_to_datetime(Start)]),
  StartBin = integer_to_binary(Start - State#state.epoch_start),
  State#state{
    candles_start = Start,
    candles_start_bin = StartBin,
    candles_start_utc = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    empty = true,
    current_tref = TRef,
    candles_last_flushed = Start}.

%%--------------------------------------------------------------------
flush_candles(State) ->
  DT = calendar:gregorian_seconds_to_datetime(State#state.candles_start),
  Data = ets:tab2list(State#state.tid),
  [online_history_worker:add_recent_candle(State#state.history_name, C) || C <- Data],
  refire_on_flush_candles(State),
  ok = candles_cached_store:store(State#state.cache_context, DT, Data).

%%--------------------------------------------------------------------
candle_to_memcached(#candle{name = N, open = O, high = H, low = L, close = C, vol = V, smas = SMAs}, State) ->
  Instr = case is_binary(N) of
            true -> N;
            false -> list_to_binary(N)
          end,
  Key = <<Instr/binary, ",", (State#state.name_bin)/binary>>,
  {Mega, Sec, Micro} = erlang:now(),
  TSB = integer_to_binary(Micro + Sec * 1000000 + Mega * 1000000000000),
  OB = float_to_binary(O, [{decimals, 4}]),
  HB = float_to_binary(H, [{decimals, 4}]),
  LB = float_to_binary(L, [{decimals, 4}]),
  CB = float_to_binary(C, [{decimals, 4}]),
  VB = integer_to_binary(V),

  SMAText = iolist_to_binary(
    [
      [",\"", T, "\":\"", float_to_binary(SV, [{decimals, 4}]), "\""]
      || {T, SV} <- SMAs
    ]),
  Data = <<
  "{\"timestamp\":",
  TSB/binary,
  ",",
  "\"data\":{",
  "\"name\":",
  "\"",
  Instr/binary,
  "\",",
  "\"ts\":",
  "\"",
  (State#state.candles_start_bin)/binary,
  "\",",
  "\"open\":",
  "\"",
  OB/binary,
  "\",",
  "\"high\":",
  "\"",
  HB/binary,
  "\",",
  "\"low\":",
  "\"",
  LB/binary,
  "\",",
  "\"close\":",
  "\"",
  CB/binary,
  "\",",
  "\"volume\":",
  "\"",
  VB/binary,
  "\"",
  SMAText/binary,
  "}}"
  >>,
  erlmc:set(Key, Data).

%%--------------------------------------------------------------------
log_expired_ticks(State = #state{expired_ticks = T}, Limit) when T > Limit ->
  lager:warning("Catch EXPIRED ticks: ~p", [T]),
  State#state{expired_ticks = 0};
log_expired_ticks(State, _) -> State.

%%--------------------------------------------------------------------
active_fires_fun(Candle, TickCandleTime) -> patterns_executor:check_patterns({tick, Candle}, TickCandleTime).
inactive_fires_fun(_, _) -> ok.

%%--------------------------------------------------------------------
%считает текущее время относительно времени начала свечи (которое может не совпадать с UTC, а идти с запаздыванием
universal_to_candle_time(State) ->
  calendar:datetime_to_gregorian_seconds(erlang:universaltime()) - State#state.candles_start_utc + State#state.candles_start.

%%--------------------------------------------------------------------
refire_on_flush_candles(State) ->
  CurrentTime = universal_to_candle_time(State),
  lager:info("CHECKING_FLUSH_PATTERNS (~p) at ~p", [State#state.name, CurrentTime]),
  ets:foldl(
    fun(Candle = #candle{}, _) -> patterns_executor:check_patterns({candle, Candle}, CurrentTime) end,
    undefined,
    State#state.tid),
  lager:info("CHECKING_FLUSH_PATTERNS completed"),
  ok.

%%--------------------------------------------------------------------
-spec sma_storage_name(FrameName :: atom()) -> atom().
sma_storage_name(FrameName) -> list_to_atom(atom_to_list(FrameName) ++ "_sma_table").

%%--------------------------------------------------------------------
-spec update_sma_queues(Tick :: #tick{}, Tid :: ets:tid(), KnownSMAs :: [atom()]) -> sma_values().
update_sma_queues(Tick, Tid, KnownSMAs) ->
  F =
    fun({SMAName, SMABinName}, Acc) ->
      Key = ?SMA_QUEUE_KEY(Tick#tick.name, SMAName),
      case ets:lookup(Tid, Key) of
        [] -> Acc;
        [{_, Q}] ->
          NewQ = sma_queue:store(Tick#tick.last_price, Q),
          ets:update_element(Tid, Key, {2, NewQ}),
          [{SMABinName, NewQ#sma_q.val} | Acc]
      end
    end,
  lists:foldr(F, [], KnownSMAs).