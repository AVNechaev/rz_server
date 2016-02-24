%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Feb 2016 19:05
%%%-------------------------------------------------------------------
-module(patterns_executor).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, load_pattern/1, compile_pattern/1, check_patterns/1, delete_pattern/1]).

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
  workers :: [pid()]
}).

-define(NO_DATA, no_data).
-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec load_pattern(Pat :: #pattern{}) -> ok.
load_pattern(Pat) -> gen_server:call(?SERVER, {load_pattern, Pat}).

%%--------------------------------------------------------------------
-spec delete_pattern(pattern_index()) -> ok.
delete_pattern(PatId) -> gen_server:call(?SERVER, {delete_pattern, PatId}).

%%--------------------------------------------------------------------
-spec check_patterns(InstrName :: instr_name()) -> ok.
check_patterns(Instr) -> gen_server:cast(?SERVER, {check_patterns, Instr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  Cfg = iqfeed_util:get_env(rz_server, patterns_executor),
  Workers = proplists:get_value(workers, Cfg),
  PIDs = [begin {ok, P} = pat_exec_worker:start_link(), P end || _ <- lists:seq(1, Workers)],
  {ok, #state{workers =  PIDs}}.

%%--------------------------------------------------------------------
handle_call({load_pattern, Pat}, _From, State) ->
  {ok, Fun} = compile_pattern(Pat),
  AnchoredFun = fun(Instr) ->
    try
      Fun(Instr)
    catch
      ?NO_DATA -> false
    end
  end,
  pat_exec_worker:load_pattern(elect(State), Pat, AnchoredFun),
  {reply, ok, State};
%%---
handle_call({delete_pattern, PatId}, _From, State) ->
  [pat_exec_worker:delete_pattern(Pid, PatId) || Pid <- State#state.workers],
  {reply, ok, State}.

%%--------------------------------------------------------------------
handle_cast({check_patterns, Instr}, State = #state{workers = W}) ->
  [pat_exec_worker:check_patterns(P, Instr) || P <- W],
  {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, _State) -> exit(handle_info_unsupported).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
elect(#state{workers = Workers}) -> lists:nth(random:uniform(length(Workers)), Workers).

%%--------------------------------------------------------------------
compile_pattern(#pattern{text = PatternText}) -> compile_pattern(PatternText);
compile_pattern(PatternText) ->
  {ok, Tokens, _} = patterns_lex:string(PatternText),
  {ok, ParsedPattern} = patterns_parser:parse(Tokens),
  {ok, transform_pattern(ParsedPattern)}.

%%--------------------------------------------------------------------
-spec transform_pattern(tuple()) -> pattern_fun().
transform_pattern({{two_op_logic, _, Operator}, LeftOperand, RightOperand}) ->
  lager:info("Pattern operator ~p", [Operator]),
  LeftFun = transform_pattern(LeftOperand),
  RightFun = transform_pattern(RightOperand),
  case Operator of
    op_and -> fun(Instr) -> LeftFun(Instr) andalso RightFun(Instr) end;
    op_or -> fun(Instr) -> LeftFun(Instr) orelse RightFun(Instr) end
  end;
%%---
transform_pattern({{comparator, _, Operator}, LeftOperand, RightOperand}) ->
  lager:info("Pattern operator \"~s\"", [Operator]),
  LeftFun = transform_pattern(LeftOperand),
  RightFun = transform_pattern(RightOperand),
  case Operator of
    ">" -> fun(Instr) -> LeftFun(Instr) > RightFun(Instr) end;
    "<" -> fun(Instr) -> LeftFun(Instr) < RightFun(Instr) end;
    ">=" -> fun(Instr) -> LeftFun(Instr) >= RightFun(Instr) end;
    "<=" -> fun(Instr) -> LeftFun(Instr) =< RightFun(Instr) end
  end;
%%---
transform_pattern({{two_op_arith, _, Operator}, LeftOperand, RightOperand}) ->
  lager:info("Pattern operator ~p", [Operator]),
  LeftFun = transform_pattern(LeftOperand),
  RightFun = transform_pattern(RightOperand),
  case Operator of
    op_rem   -> fun(Instr) -> LeftFun(Instr) rem RightFun(Instr) end;
    op_plus  -> fun(Instr) -> LeftFun(Instr) + RightFun(Instr) end;
    op_minus -> fun(Instr) -> LeftFun(Instr) - RightFun(Instr) end
  end;
%%---
transform_pattern({constant, _, Value}) ->
  lager:info("Pattern operand CONST=~p", [Value]),
  fun(_) -> Value end;
%%---
transform_pattern({instr, Line, Instr}) when is_list(Instr) -> transform_pattern({instr, Line, list_to_binary(Instr)});
transform_pattern({instr, _, <<"Instr#", Data/binary>>}) ->
  [FrameOff, Val] = binary:split(Data, <<"#">>),
  [Frame, Offset] = binary:split(FrameOff, <<",">>),
  FrameName = proplists:get_value(Frame, iqfeed_util:get_env(rz_server, pattern_names_to_frames)),
  CurStorageName = timeframe_worker:storage_name(FrameName),
  HistStorageName = online_history_worker:storage_name(FrameName),
  Length = proplists:get_value(history_depth, proplists:get_value(FrameName, iqfeed_util:get_env(rz_server, frames))),
  GetCandleFun = case Offset of
                   <<"1">> ->
                     lager:info("Pattern operand get_current_candle(~p, Instr)", [CurStorageName]),
                     fun(Instr) -> timeframe_worker:get_current_candle(Instr, CurStorageName) end;
                   _ ->
                     OffInt = binary_to_integer(Offset) - 2,
                   lager:info("Pattern operand get_candle(~p, Instr, ~p, ~p)", [HistStorageName, Length, OffInt]),
                     fun(Instr) ->
                       online_history_worker:get_candle(HistStorageName, Instr, Length, OffInt)
                     end
                 end,
  ExtrFun = case Val of
              <<"OPEN">>   -> fun(#candle{open = V}) -> V end;
              <<"CLOSE">>  -> fun(#candle{close = V}) -> V end;
              <<"HIGH">>   -> fun(#candle{high = V}) -> V end;
              <<"LOW">>    -> fun(#candle{low = V}) -> V end;
              <<"PRICE">>  -> fun(#candle{close = V}) -> V end;
              <<"BID">>    -> fun(#candle{bid = V}) -> V end;
              <<"ASK">>    -> fun(#candle{ask = V}) -> V end;
              <<"VOLUME">> -> fun(#candle{vol = V}) -> V end
            end,
  fun(Instr) ->
    case GetCandleFun(Instr) of
      {ok, C} -> ExtrFun(C);
      {error, not_found} -> throw (?NO_DATA)
    end
  end.
