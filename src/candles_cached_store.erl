%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Февр. 2016 19:58
%%%-------------------------------------------------------------------
-module(candles_cached_store).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/3, store/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("internal.hrl").

-record(state, {
  cache = [] :: [{Name :: atom(), DT :: calendar:datetime(), Candle :: #candle{}}],
  max_size :: non_neg_integer(),
  timeout :: pos_integer(),
  destinations :: [{Name :: atom(), StatName :: atom()}]
}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(
    Tables :: [{Name :: atom(), TableName :: string()}],
    MaxSize :: non_neg_integer(),
    Timeout :: pos_integer()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Tables, MaxSize, Timeout) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Tables, MaxSize, Timeout], []).

%%--------------------------------------------------------------------
-spec store(Dest :: atom(), DateTime :: calendar:datetime(), Candles :: [#candle{}]) -> ok.
store(Dest, DateTime, Candles) -> gen_server:cast(?SERVER, {store, Dest, DateTime, Candles}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Tables, MaxSize, Timeout]) ->
  Dests = lists:map(
    fun({Name, TableName}) ->
      StatName = list_to_atom(atom_to_list(Name) ++ "_stmt"),
      emysql:prepare(
        StatName,
        "INSERT INTO " ++ TableName ++ " (name, ts, open, high, low, close, volume) VALUES (?,?,?,?,?,?,?)"),
      {Name, StatName}
    end,
    Tables),
  {ok, #state{max_size = MaxSize, timeout = Timeout, destinations = Dests}}.

%%--------------------------------------------------------------------
handle_cast({store, Dest, DateTime, Candles}, State) ->
  CurZipped = [{Dest, DateTime, C} || C <- Candles],
  NewCached = CurZipped ++ State#state.cache,
  if
    erlang:length(NewCached) >= State#state.max_size ->
      flush(NewCached, State),
      {noreply, State#state{cache = []}};
    true ->
      erlang:send_after(State#state.timeout, self(), timeout),
      {noreply, State#state{cache = NewCached}}
  end.

%%--------------------------------------------------------------------
handle_info(timeout, State = #state{cache = []}) -> {noreply, State};
handle_info(timeout, State) ->
  flush(State#state.cache, State),
  {noreply, State#state{cache = []}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
flush(Data, State) ->
  lager:info("Flushing candles cache..."),
  StoreFun = fun({Name, DT, C}) ->
    VV = [
      C#candle.name,
      ?DATETIME_TO_MYSQL(DT),
      C#candle.open,
      C#candle.high,
      C#candle.low,
      C#candle.close,
      C#candle.vol
    ],
    lager:info("FLUSH REC: ~p", [VV]),
    emysql:execute(
      mysql_candles_store,
      proplists:get_value(Name, State#state.destinations),
      VV
    )
  end,
  lists:foreach(StoreFun, Data),
  ok.
