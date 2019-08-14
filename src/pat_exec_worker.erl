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
-export([start_link/0, load_pattern/3, check_patterns/3, delete_pattern/2, load_timecodes_data/2]).

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
  f :: pattern_fun(),
  vf :: var_fun()
}).

-define(FIRES_ID(PatId, Instr), {PatId, Instr}).
-record(fires_data, {
  id :: {pattern_index(), instr_name()},
  last_fired :: pos_integer()
}).

-type tctime() :: non_neg_integer() | calendar:time().
-type timecode() :: {TCId :: binary(), {From :: tctime(), To :: tctime()}}.

-export_type([timecode/0]).

-record(state, {
  tid :: ets:tid(),
  patterns :: [#pattern_data{}],
  refire_timeout :: non_neg_integer(),
  timecodes :: map()
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
  RefireTimeout = proplists:get_value(refire_timeout, rz_util:get_env(rz_server, patterns_executor)),
  TCData = rz_util:get_env(rz_server, timecodes),
  {ok, #state{tid = Tid, patterns = [], refire_timeout = RefireTimeout, timecodes = load_timecodes_data(TCData)}}.

%%--------------------------------------------------------------------
-spec load_pattern(ThisPid :: pid(), PatDesc :: #pattern{}, {PatFun :: pattern_fun(), VarFun :: var_fun()}) -> ok.
load_pattern(ThisPid, PatDesc, {PatFun, VarFun}) -> gen_server:call(ThisPid, {load_pattern, PatDesc, {PatFun, VarFun}}).

%%--------------------------------------------------------------------
-spec delete_pattern(ThisPid :: pid(), PatId :: pattern_index()) -> ok.
delete_pattern(ThisPid, PatId) -> gen_server:call(ThisPid, {delete_pattern, PatId}).

%%--------------------------------------------------------------------
-spec check_patterns(ThisPid :: pid(), Instr :: pattern_fun_arg(), UTCCandlesTime :: pos_integer()) -> ok.
check_patterns(ThisPid, Instr, UTCCandlesTime) -> gen_server:cast(ThisPid, {check_patterns, Instr, UTCCandlesTime}).

%%--------------------------------------------------------------------
-spec load_timecodes_data(ThisPid :: pid(), TCData :: [timecode()]) -> ok.
load_timecodes_data(ThisPid, TCData) -> gen_server:call(ThisPid, {load_timecodes_data, TCData}).

%%--------------------------------------------------------------------
handle_call({load_pattern, #pattern{idx = Id, text = Txt}, {PatFun, VarFun}}, _From, State) ->
  lager:info("Loading pattern #~p: ~p", [Id, Txt]),
  NewPatterns = [#pattern_data{id = Id, f = PatFun, vf = VarFun} | State#state.patterns],
  {reply, ok, State#state{patterns = NewPatterns}};
handle_call({delete_pattern, PatId}, _From, State) ->
  NewPatterns = lists:keydelete(PatId, #pattern_data.id, State#state.patterns),
  case length(NewPatterns) of
    A when A == length(State#state.patterns) -> ok;
    _ -> lager:info("Pattern ~p deleted", [PatId])
  end,
  {reply, ok, State#state{patterns = NewPatterns}};
handle_call({load_timecodes_data, TCData}, _From, State) ->
  {reply, ok, State#state{timecodes = load_timecodes_data(TCData)}}.

%%--------------------------------------------------------------------
handle_cast({check_patterns, Instr = {_, _, InstrName}, UTCCandlesTime}, State = #state{tid = Tid, refire_timeout = Timeout}) ->
  TCCheckFun = fun(TC) -> filter_by_timecode(TC, State#state.timecodes) end,
  F =
    fun(#pattern_data{id = Id, f = PatFun, vf = VarFun}) ->
      FiresId = ?FIRES_ID(Id, InstrName),
      case ets:lookup(Tid, FiresId) of
        [#fires_data{last_fired = LF}] when UTCCandlesTime - LF > Timeout ->
          case PatFun(Instr, TCCheckFun) of
            true ->
              on_fired(Id, Instr, VarFun, UTCCandlesTime, State),
              true = ets:update_element(Tid, FiresId, [{#fires_data.last_fired, UTCCandlesTime}]),
              ok;
            false ->
              ok
          end;
        [#fires_data{}] -> ok;
        [] ->
          case PatFun(Instr, TCCheckFun) of
            true ->
              on_fired(Id, Instr, VarFun, UTCCandlesTime, State),
              true = ets:insert_new(Tid, #fires_data{id = FiresId, last_fired = UTCCandlesTime}),
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
on_fired(PatIdx, {From, Frame, Instr}, VarFun, UTCCandlesTime, _State) ->
  VarText = lists:map(
    fun({Name, undefined}) ->
      [Name, "=undefined;"];
      ({Name, IntVal}) when is_integer(IntVal) ->
        [Name, io_lib:format("=~f;", [float(IntVal)])];
      ({Name, Val}) ->
        [Name, io_lib:format("=~f;", [Val])]
    end,
    VarFun(Instr)
  ),
  lager:debug("PATTERN ~p fired for {~p,~p; ~p} at ~p", [PatIdx, From, {Frame, Instr}, VarText, UTCCandlesTime]),
  metrics:add_fire(),
  fires_cached_store:store(PatIdx, Instr, UTCCandlesTime, VarText),
  ok.

%%--------------------------------------------------------------------
load_timecodes_data(TCData) ->
  lists:foldr(
    fun
      ({TC, {{Hf, Mf, Sf}, {Ht, Mt, St}}}, Map) -> maps:put(TC, {Hf*60*60 + Mf*60 + Sf, Ht*60*60 + Mt*60 + St}, Map);
      ({TC, {From, To}}, Map) -> maps:put(TC, {From, To}, Map)
    end,
    maps:new(),
    TCData).

%%--------------------------------------------------------------------
filter_by_timecode(undefined, _) -> true;
filter_by_timecode({timecode, TC}, TCStore) ->
  case maps:find(TC, TCStore) of
    {ok, {From, To}} ->
      {_, {H, M, S}} = erlang:localtime(),
      Seconds = H*60*60 + M*60 + S,
      if
        Seconds >= From andalso Seconds =< To -> true;
        true -> false
      end;
    _ ->
      false
  end.