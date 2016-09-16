%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jul 2016 23:51
%%%-------------------------------------------------------------------
-module(sma_store).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, add_daily_candle/1, get_sma/2, get_current_sma/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include_lib("rz_util/include/rz_util.hrl").

-define(SMA_KEY(Instr, Type), {sma, Instr, Type}). %% здесь хранятся последние посчитанные SMA
-define(HISTORY_KEY(Instr, Type), {hist, Instr, Type}). %% здесь хранятся свечки up to depth
-define(STORE_NAME, ets_sma_store).
-record(state, {known_smas :: [{Type :: atom(), Depth :: pos_integer()}]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec add_daily_candle(#candle{}) -> ok.
add_daily_candle(Candle) -> gen_server:call(?SERVER, {add_daily_candle, Candle}).

%%--------------------------------------------------------------------
-spec get_sma(Instr :: instr_name(), Type :: atom()) -> {ok, Data :: float()} | {error, not_found}.
get_sma(Instr, Type) ->
  case ets:lookup(?STORE_NAME, ?SMA_KEY(Instr, Type)) of
    [] -> {error, not_found};
    [{_, Data}] -> {ok, Data}
  end.

%%--------------------------------------------------------------------
-spec get_current_sma(Instr :: instr_name(), LastPrice :: float(), Type :: atom()) -> {ok, Data :: float()} | {error, not_found}.
get_current_sma(Instr, LastPrice, Type) ->
  case ets:lookup(?STORE_NAME, ?SMA_KEY(Instr, Type)) of
    [] -> {error, not_found};
    [{_, Data}] ->
      [{_, Q}] = ets:lookup(?STORE_NAME, ?HISTORY_KEY(Instr, Type)),
      Len = queue:len(Q),
      Last = queue:get(Q),
      {ok, Data + (LastPrice - Last) / Len}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?STORE_NAME, [protected, named_table, set, {read_concurrency, true}]),
  SMAConf = rz_util:get_env(rz_server, sma_store),
  {ok, #state{known_smas = proplists:get_value(known, SMAConf)}}.

%%--------------------------------------------------------------------
handle_call({add_daily_candle, Candle}, _From, State = #state{known_smas = Known}) ->
  lists:foreach(fun(I) -> process_single(Candle, I) end, Known),
  {reply, ok, State}.

%%--------------------------------------------------------------------
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
handle_info(_Info, _State) -> exit(handle_info_unsupported).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_single(#candle{name = Instr, close = Data}, {SmaType, Depth}) ->
  Key = ?HISTORY_KEY(Instr, SmaType),
  Q =
    case ets:lookup(?STORE_NAME, Key) of
      [] ->
        ets:insert_new(?STORE_NAME, {Key, undefined}), %%затрется при update_element
        queue:new();
      [{_, D}] -> D
    end,
  {Dropped, NewQ} =
    case queue:len(Q) of
      Depth ->
        {{value, Drop}, Q1} = queue:out(Q),
        NQ = queue:in(Data, Q1),
        {Drop, NQ};
      LM when LM < Depth ->
        {0, queue:in(Data, Q)}
    end,
  case queue:len(Q) of
    Depth ->
      recalc_sma(Instr, Dropped, NewQ, SmaType);
    _ -> ok
  end,
  true = ets:update_element(?STORE_NAME, Key, {2, NewQ}),
  ok.

%%--------------------------------------------------------------------
recalc_sma(Instr, Dropped, NQ, SmaType) ->
  L = queue:len(NQ),
  Key = ?SMA_KEY(Instr, SmaType),
  Res =
    case ets:lookup(?STORE_NAME, Key) of
      [] -> lists:foldl(fun(D, Acc) -> Acc + D / L end, 0, queue:to_list(NQ));
      [{_, V}] -> V + (queue:get_r(NQ) - Dropped) / L
    end,
  ets:insert(?STORE_NAME, {Key, Res}),
  sma_to_memcached(Instr, SmaType, L, Res, queue:get(NQ)).

%%--------------------------------------------------------------------
sma_to_memcached(Instr, SmaType, Length, V, Last) ->
  SmaBin = atom_to_binary(SmaType, latin1),
  Key = <<Instr/binary, ",", SmaBin/binary>>,
  {Mega, Sec, Micro} = erlang:now(),
  TSB = integer_to_binary(Micro + Sec * 1000000 + Mega * 1000000000000),
  LenBin = integer_to_binary(Length),
  VBin = float_to_binary(V, [{decimals, 4}]),
  LastBin = float_to_binary(Last, [{decimals, 4}]),
  Data = <<
  "{\"timestamp\":",
  TSB/binary,
  ",",
  "\"data\":{",
  "\"v\":",
  "\"",
  VBin/binary,
  "\",",
  "\"len\":",
  "\"",
  LenBin/binary,
  "\",",
  "\"last\":",
  "\"",
  LastBin/binary,
  "\"",
  "}}"
  >>,
  erlmc:set(Key, Data).