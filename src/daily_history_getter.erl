%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2016 10:19
%%%-------------------------------------------------------------------
-module(daily_history_getter).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, get_history_for/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

-record(req, {
  instrs :: [instr_name()],
  depth :: non_neg_integer(),
  f :: on_history_fun()
}).
-record(state, {
  cur_req = undefined :: undefined | #req{},
  retries_timeout :: non_neg_integer()}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec get_history_for(Instr :: [instr_name()], Depth :: pos_integer(), Fun :: on_history_fun()) -> ok | {error, busy}.
get_history_for(Instrs, Depth, Fun) -> gen_server:call(?SERVER, {get, Instrs, Depth, Fun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{retries_timeout = 3000}}.

%%--------------------------------------------------------------------
handle_call({get, _Instrs, _Depth, _Fun}, _From, State = #state{cur_req = Req}) when Req =/= undefined ->
  {reply, {error, busy}, State};
handle_call({get, Instrs, Depth, Fun}, _From, State) ->
  lager:info("Start getting daily history for ~p instruments", [erlang:length(Instrs)]),
  gen_server:cast(self(), activate),
  {reply, ok, State#state{cur_req = #req{depth = Depth, f = Fun, instrs = Instrs}}}.

%%--------------------------------------------------------------------
handle_cast(activate, State = #state{cur_req = #req{instrs = []}}) ->
  lager:info("Done getting daily history"),
  {noreply, State#state{cur_req = undefined}};
handle_cast(activate, State = #state{cur_req = #req{instrs = [H | T], f = F, depth = D}}) ->
  Self = self(),
  case iql2_conn:get_history(H, D, fun(D) -> hist_fun(Self, F, D) end) of
    ok ->
      NewReq = (State#state.cur_req)#req{instrs = T},
      {noreply, State#state{cur_req = NewReq}};
    {error, not_connected} ->
      erlang:start_timer(State#state.retries_timeout, self, activate),
      {noreply, State}
  end.

%%--------------------------------------------------------------------
handle_info({timeout, _, activate}, State) -> handle_cast(activate, State).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
hist_fun(_Self, ReqFun, Data = {data, _, _}) -> ReqFun(Data);
hist_fun(Self, ReqFun, ErrorOrEndOfData) ->
  ReqFun(ErrorOrEndOfData),
  gen_server:cast(Self, activate).