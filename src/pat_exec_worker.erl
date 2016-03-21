%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Feb 2016 19:11
%%%-------------------------------------------------------------------
-module(pat_exec_worker).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, load_pattern/3, check_patterns/2, delete_pattern/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal.hrl").
-record(pattern_data, {
  id :: pattern_index(),
  f :: pattern_fun()
}).

-define(FIRES_ID(PatId, Instr), {PatId, Instr}).
-record(fires_data, {
  id :: {pattern_index(), instr_name()},
  last_fired :: pos_integer()
}).

-record(state, {
  tid :: ets:tid(),
  patterns :: [#pattern_data{}],
  refire_timeout :: non_neg_integer()
}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  Tid = ets:new(?MODULE, [private, set, {keypos, #fires_data.id}]),
  RefireTimeout = proplists:get_value(refire_timeout, iqfeed_util:get_env(rz_server, patterns_executor)),
  {ok, #state{tid = Tid, patterns = [], refire_timeout = RefireTimeout}}.

%%--------------------------------------------------------------------
-spec load_pattern(ThisPid :: pid(), PatDesc :: #pattern{}, PatFun :: pattern_fun()) -> ok.
load_pattern(ThisPid, PatDesc, PatFun) -> gen_server:call(ThisPid, {load_pattern, PatDesc, PatFun}).

%%--------------------------------------------------------------------
-spec delete_pattern(ThisPid :: pid(), PatId :: pattern_index()) -> ok.
delete_pattern(ThisPid, PatId) -> gen_server:call(ThisPid, {delete_pattern, PatId}).

%%--------------------------------------------------------------------
-spec check_patterns(ThisPid :: pid(), Instr :: instr_name()) -> ok.
check_patterns(ThisPid, Instr) -> gen_server:cast(ThisPid, {check_patterns, Instr}).

%%--------------------------------------------------------------------
handle_call({load_pattern, #pattern{idx = Id, text = Txt}, PatFun}, _From, State) ->
  lager:info("Loading pattern #~p: ~p", [Id, Txt]),
  NewPatterns = [#pattern_data{id = Id, f = PatFun} | State#state.patterns],
  {reply, ok, State#state{patterns = NewPatterns}};
handle_call({delete_pattern, PatId}, _From, State) ->
  NewPatterns = lists:keydelete(PatId, #pattern_data.id, State#state.patterns),
  {reply, ok, State#state{patterns = NewPatterns}}.

%%--------------------------------------------------------------------
handle_cast({check_patterns, Instr}, State = #state{tid = Tid, refire_timeout = Timeout}) ->
  NowSecUTC = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
  F =
    fun(#pattern_data{id = Id, f = PatFun}) ->
      FiresId = ?FIRES_ID(Id, Instr),
      case ets:lookup(Tid, FiresId) of
        [#fires_data{last_fired = LF}] when NowSecUTC - LF > Timeout ->
          case PatFun(Instr) of
            true ->
              on_fired(Id, Instr, State),
              true = ets:update_element(Tid, FiresId, [{#fires_data.last_fired, NowSecUTC}]),
              ok;
            false ->
              ok
          end;
        [] ->
          case PatFun(Instr) of
            true ->
              on_fired(Id, Instr, State),
              true = ets:insert_new(Tid, #fires_data{id = FiresId, last_fired = NowSecUTC}),
              ok;
            false ->
              ok
          end
      end
    end,
  lists:foreach(F, State#state.patterns),
  {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, _State) -> exit(handle_info_unsupported).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
on_fired(PatIdx, Instr, _State) ->
  lager:info("PATTERN ~p fired for ~p at ~p", [PatIdx, Instr, erlang:now()]),
  fires_cached_store:store(PatIdx, Instr),
  ok.
