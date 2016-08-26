%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2016 19:54
%%%-------------------------------------------------------------------
-module(sma_updater).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/0, add_daily_candle/2, exec/1]).

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
-define(STORE_NAME, ets_sma_updater).
-record(state, {known_smas :: [{Type :: atom(), Depth :: pos_integer()}]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec exec(Depth :: pos_integer()) -> ok.
exec(Depth) ->
  gen_server:call(?SERVER, clean_ets),
  {ok, Instr} = rz_util:load_instr_csv(
    rz_util:get_env(iqfeed_client, instr_file),
    rz_util:get_env(iqfeed_client, instr_file_header),
    rz_util:get_env(iqfeed_client, instr_defaults)
  ),
  F =
    fun({data, {Time, Candle}}) -> add_daily_candle(Time, Candle);
      (_) -> ok
    end,
  daily_history_getter:get_history_for(Instr, Depth, F).

%%--------------------------------------------------------------------
-spec add_daily_candle(Timestamp :: pos_integer(), #candle{}) -> ok.
add_daily_candle(Timestamp, Candle) -> gen_server:call(?SERVER, {add_daily_candle, Timestamp, Candle}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?STORE_NAME, [protected, named_table, set, {read_concurrency, true}]),
  SMAConf = rz_util:get_env(rz_server, sma_updater),
  [_|SQL] = lists:flatten([[",", I, "=?"] || {_, _, I} <- SMAConf]),
  emysql:prepare(sma_updater, "UPDATE CANDLES_DAILY SET " ++ SQL ++ " WHERE name=? AND ts=?"),
  {ok, #state{known_smas = rz_util:get_env(rz_server, sma_updater)}}.

%%--------------------------------------------------------------------
handle_call(clean_ets, _From, State) ->
  ets:delete_all_objects(?STORE_NAME),
  {reply, ok, State};
%%---
handle_call({add_daily_candle, Timestamp, Candle}, _From, State = #state{known_smas = Known}) ->
  lists:foreach(fun(I) -> process_single(Candle, I) end, Known),
  update_db(Timestamp, Candle#candle.name, Known),
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
process_single(#candle{name = Instr, close = Data}, {SmaType, Depth, _FieldName}) ->
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
  {ok, Res}.

%%--------------------------------------------------------------------
get_v(Instr, Type) ->
  case ets:lookup(?STORE_NAME, ?SMA_KEY(Instr, Type)) of
    [] -> 0.0;
    [{_, Data}] -> Data
  end.

%%--------------------------------------------------------------------
update_db(Timestamp, Instr, Known) ->
  Vals =
    [get_v(Instr, Type) || {Type, _, _} <- Known] ++
    [
      Instr,
      util:datetime_to_mysql(calendar:gregorian_seconds_to_datetime(Timestamp))
    ],
  emysql:execute(mysql_candles_store, sma_updater, Vals).