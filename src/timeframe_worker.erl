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

-include("internal.hrl").
-record(state, {
  empty :: boolean(),
  trading_start :: calendar:time(),
  candles_start :: integer(),
  candles_start_bin :: binary(),
  tid :: ets:tid(),
  current_tref = undefined :: undefined | reference(),
  duration :: pos_integer(), %длительность свечки
  name :: atom(),
  name_bin :: binary(),
  history_name :: atom()
}).

-type frame_params() :: list().

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
  Tid = ets:new(storage_name(Name), [named_table, protected, set, {keypos, #candle.name}]),
  {ok,
    #state{
      empty = true,
      tid = Tid,
      trading_start = iqfeed_util:get_env(rz_server, trading_start),
      duration = proplists:get_value(duration, Params),
      name = Name,
      name_bin = atom_to_binary(Name, latin1),
      history_name = online_history_worker:reg_name(Name)
    }}.

%%--------------------------------------------------------------------
handle_cast({add_tick, Tick}, State = #state{empty = true}) ->
  NewState = reinit_state(Tick#tick.time, State),
  update_current_candle(Tick, NewState),
  {noreply, NewState#state{empty = false}};
%%---
handle_cast({add_tick, Tick}, State) ->
  NewState = if
               (Tick#tick.time - State#state.candles_start) >= State#state.duration ->
                 reinit_state(Tick#tick.time, State);
               true -> State
             end,
  update_current_candle(Tick, NewState),
  {noreply, NewState#state{empty = false}}.

%%--------------------------------------------------------------------
handle_info({timeout, _, reinit}, State = #state{empty = true}) -> {noreply, State};
%% этот вариант возможен, когда в 1 секунду сначала придет отсчет, пересчитается новый таймер, а потом сработает старый
handle_info({timeout, TRef, reinit}, State = #state{current_tref = OtherTRef}) when TRef =/= OtherTRef ->
  {noreply, State};
handle_info({timeout, _, reinit}, State) ->
  flush_candles(State),
  ets:delete_all_objects(State#state.tid),
  {noreply, State#state{empty = true}}.

%%--------------------------------to_date------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_current_candle(#tick{name = Name, last_price = LP, last_vol = LV, bid = Bid, ask = Ask}, State = #state{tid = Tid}) ->
  case ets:lookup(Tid, Name) of
    [] ->
      NewCandle = #candle{name = Name, open = LP, close = LP, high = LP, low = LP, vol = LV},
      candle_to_memcached(NewCandle, State#state.name_bin, State#state.candles_start_bin),
      true = ets:insert_new(Tid, NewCandle);
    [C] ->
      U1 = [{#candle.close, LP}, {#candle.vol, C#candle.vol + LV}, {#candle.bid, Bid}, {#candle.ask, Ask}],
      U2 = if
             LP > C#candle.high -> [{#candle.high, LP} | U1];
             true -> U1
           end,
      U3 = if
             LP < C#candle.low -> [{#candle.low, LP} | U2];
             true -> U2
           end,
%%       TODO: заменить предыдущую свечу на текущую...
      candle_to_memcached(C, State#state.name_bin, State#state.candles_start_bin),
      ets:update_element(Tid, C#candle.name, U3)
  end.

%%--------------------------------------------------------------------
reinit_state(TickTime, State = #state{duration = Duration, trading_start = TradingStart}) ->
  case State#state.empty of
    false -> flush_candles(State);
    true -> ok
  end,
  ets:delete_all_objects(State#state.tid),
  {D, _} = calendar:gregorian_seconds_to_datetime(TickTime),

  ExpectedStart = (TickTime div Duration) * Duration,
  TradingStartSeconds = calendar:datetime_to_gregorian_seconds({D, TradingStart}),
  Start = if
            ExpectedStart < TradingStartSeconds -> TradingStartSeconds;
            true -> ExpectedStart
          end,
  % если на момент срабатывания таймера есть еще тики в очереди, то надо сначала
  % обработать их; иначе при флуде тиков происходит повторная инициализация свечки
  % как вариант, можно при достаточной пропускной способности поставить
  % в reinit_timer (duration + 1), думая, что за секунду разгребутся остатки
  % тиков предыдущей секунды
  TRef = erlang:start_timer((Duration + 1) * 1000, self(), reinit),
  lager:info("REINIT CANDLE DURATON ~p AT ~p", [State#state.duration, calendar:gregorian_seconds_to_datetime(Start)]),
  DT = util:datetime_to_mysql(calendar:gregorian_seconds_to_datetime(Start)),
  State#state{candles_start = Start, candles_start_bin = DT, empty = true, current_tref = TRef}.

%%--------------------------------------------------------------------
flush_candles(State) ->
  DT = calendar:gregorian_seconds_to_datetime(State#state.candles_start),
  {{Y, M, D}, {H, Mi, S}} = DT,
  Data = ets:tab2list(State#state.tid),
  [
    begin
      lager:info(
        "Candle-~p,~p.~p.~p ~p:~p:~p,~p,~p,~p,~p,~p,~p",
        [State#state.name, D, M, Y, H, Mi, S, C#candle.name, C#candle.open, C#candle.close, C#candle.high, C#candle.low, C#candle.vol]),
      online_history_worker:add_recent_candle(State#state.history_name, C)
    end || C <- Data
  ],
  ok = candles_cached_store:store(State#state.name, DT, Data).

%%--------------------------------------------------------------------
candle_to_memcached(#candle{name = N, open = O, high = H, low = L, close = C, vol = V}, NameBin, CandleStartBin) ->
  Instr = case is_binary(N) of
            true -> N;
            false -> list_to_binary(N)
          end,
  Key = <<Instr/binary, ",", NameBin/binary>>,
  {Mega, Sec, Micro} = erlang:now(),
  TSB = integer_to_binary(Micro + Sec * 1000000 + Mega * 1000000000000),
  OB = float_to_binary(O, [{decimals, 4}]),
  HB = float_to_binary(H, [{decimals, 4}]),
  LB = float_to_binary(L, [{decimals, 4}]),
  CB = float_to_binary(C, [{decimals, 4}]),
  VB = integer_to_binary(V),
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
  CandleStartBin/binary,
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
  "}}"
  >>,
  erlmc:set(Key, Data).
