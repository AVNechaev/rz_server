%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Февр. 2016 19:58
%%%-------------------------------------------------------------------
-module(fires_cached_store).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/2, store/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(ADD_STMT, ?SERVER).

-include("internal.hrl").

-record(cache_item, {
  pat_id :: pattern_index(),
  instr :: instr_name(),
  ts :: binary()
}).

-record(state, {
  cache = [] :: [#cache_item{}],
  max_size :: non_neg_integer(),
  timeout :: pos_integer()
}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(
    MaxSize :: non_neg_integer(),
    Timeout :: pos_integer()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MaxSize, Timeout) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxSize, Timeout], []).

%%--------------------------------------------------------------------
-spec store(PatId :: pattern_index(), Instr :: instr_name()) -> ok.
store(PatId, Instr) -> gen_server:cast(?SERVER, {store, PatId, Instr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MaxSize, Timeout]) ->
  emysql:prepare(?ADD_STMT, <<"insert into FIRES (id, instr, ts) VALUES (?, ?, ?)">>),
  {ok, #state{max_size = MaxSize, timeout = Timeout}}.

%%--------------------------------------------------------------------
handle_cast({store, PatId, Instr}, State) ->
  Item = #cache_item{
    pat_id = PatId,
    instr = Instr,
    ts = util:datetime_to_mysql(erlang:localtime())
  },
  NewCached = [Item | State#state.cache],
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
  StoreFun = fun(Item) ->
    VV = [
      Item#cache_item.pat_id,
      Item#cache_item.instr,
      Item#cache_item.ts
    ],
    emysql:execute(
      mysql_candles_store,
      ?ADD_STMT,
      VV
    )
  end,
  lists:foreach(StoreFun, Data),
  ok.
