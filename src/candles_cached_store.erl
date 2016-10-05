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
-export([start_link/0, store/3, init_context/1]).

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
  timeout :: pos_integer()
}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec store(Dest :: atom(), DateTime :: calendar:datetime(), Candles :: [#candle{}]) -> ok.
store(Dest, DateTime, Candles) -> gen_server:cast(?SERVER, {store, Dest, DateTime, Candles}).

%%--------------------------------------------------------------------
%% Context aka statement name
-spec init_context(FrameName :: atom()) -> atom().
init_context(FrameName) -> list_to_atom(atom_to_list(FrameName) ++ "_stmt").

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  Tables = rz_util:get_env(rz_server, cache_tables),
  MaxSize = rz_util:get_env(rz_server, cache_size),
  Timeout = rz_util:get_env(rz_server, cache_timeout),
  Fields = [[",", FieldName] || {_Type, FieldName, _Depth} <- rz_util:get_env(rz_server, sma)],
  lists:map(
    fun({Name, TableName, _Options}) ->
      StmtName = init_context(Name),
      emysql:prepare(
        StmtName,
        [
          "INSERT INTO ",
          TableName,
          " (name, ts, open, high, low, close, volume",
          Fields,
          ") VALUES (?,?,?,?,?,?,?",
          lists:duplicate(",?", erlang:size(rz_util:get_env(rz_server, sma))),
          ")"]
      )
    end,
    Tables),
  {ok, #state{max_size = MaxSize, timeout = Timeout}}.

%%--------------------------------------------------------------------
handle_cast({store, CacheCtx, DateTime, Candles}, State) ->
  CurZipped = [{CacheCtx, DateTime, C} || C <- Candles],
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
flush(Data, _State) ->
  lager:info("Flushing candles cache..."),
  StoreFun = fun({CacheCtx, DT, C}) ->
    VV = [
      C#candle.name,
      util:datetime_to_mysql(DT),
      C#candle.open,
      C#candle.high,
      C#candle.low,
      C#candle.close,
      C#candle.vol
    ],
    SMAValues = [V || {_, _, V} <- C#candle.smas],
    emysql:execute(
      mysql_candles_store,
      CacheCtx,
      VV ++ SMAValues
    )
  end,
  lists:foreach(StoreFun, Data),
  ok.
