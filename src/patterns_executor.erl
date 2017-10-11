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
-export([start_link/0, load_pattern/1, compile_pattern/1, check_patterns/2, delete_pattern/1]).

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
-spec load_pattern(Pat :: #pattern{}) -> ok | {error, Reason :: term()}.
load_pattern(Pat) -> gen_server:call(?SERVER, {load_pattern, Pat}).

%%--------------------------------------------------------------------
-spec delete_pattern(pattern_index()) -> ok.
delete_pattern(PatId) -> gen_server:call(?SERVER, {delete_pattern, PatId}).

%%--------------------------------------------------------------------
%% см timeframe_worker:universal_to_candle_time()
-spec check_patterns(Instr :: pattern_fun_arg(), UniversalCandleTime :: pos_integer()) -> ok.
check_patterns(Instr, UniversalCandleTime) -> gen_server:cast(?SERVER, {check_patterns, Instr, UniversalCandleTime}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  Cfg = rz_util:get_env(rz_server, patterns_executor),
  Workers = proplists:get_value(workers, Cfg),
  PIDs = [begin {ok, P} = pat_exec_worker:start_link(), P end || _ <- lists:seq(1, Workers)],
  {ok, #state{workers = PIDs}}.

%%--------------------------------------------------------------------
handle_call({load_pattern, Pat}, _From, State) ->
  try
    {ok, {Fun, Ctx}} = compile_pattern(Pat),
    lager:info("Pattern compiled; Context: ~p", [Ctx]),
    ReferencedFrames = proplists:get_value(referenced_frames, Ctx, []),
    UsingCurrentCandle = proplists:get_value(use_current_candle, Ctx, false),
    AnchoredFun =
      fun({tick, _FrameName, Instr}) ->
        case UsingCurrentCandle of
          true ->
            try
              Fun(Instr)
            catch
              ?NO_DATA -> false
            end;
          false ->
            false
        end;
        ({candle, FrameName, Instr}) ->
          try
            frame_filter_fun(Fun, ReferencedFrames, FrameName, Instr)
          catch
            ?NO_DATA -> false
          end
      end,
    pat_exec_worker:load_pattern(elect(State), Pat, AnchoredFun),
    {reply, ok, State}
  catch
    M:E ->
      {reply, {error, {M, E, erlang:get_stacktrace()}}, State}
  end;
%%---
handle_call({delete_pattern, PatId}, _From, State) ->
  [pat_exec_worker:delete_pattern(Pid, PatId) || Pid <- State#state.workers],
  {reply, ok, State}.

%%--------------------------------------------------------------------
handle_cast({check_patterns, Instr, UTCCandlesTime}, State = #state{workers = W}) ->
  [pat_exec_worker:check_patterns(P, Instr, UTCCandlesTime) || P <- W],
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
frame_filter_fun(Fun, UsedFrames, CurrentFrame, Instr) ->
  case lists:member(CurrentFrame, UsedFrames) of
    true -> Fun(Instr);
    false -> false
  end.

%%--------------------------------------------------------------------
compile_pattern(#pattern{text = PatternText}) -> compile_pattern(PatternText);
compile_pattern(PT) when is_binary(PT) -> compile_pattern(binary_to_list(PT));
compile_pattern(PatternText) ->
  {ok, Tokens, _} = patterns_lex:string(PatternText),
  {ok, {ParsedPattern, ParsedVariables}} = patterns_parser:parse(Tokens),
  {ok, transform_pattern(ParsedPattern, []), transform_variables(ParsedVariables)}.

%%--------------------------------------------------------------------
-spec transform_pattern(tuple(), list()) -> {pattern_fun(), list()}.
transform_pattern({{two_op_logic, _, Operator}, LeftOperand, RightOperand}, Ctx) ->
  lager:info("Pattern operator ~p", [Operator]),
  {LeftFun, LCtx} = transform_pattern(LeftOperand, Ctx),
  {RightFun, NewCtx} = transform_pattern(RightOperand, LCtx),
  case Operator of
    op_and -> {fun(Instr) -> LeftFun(Instr) andalso RightFun(Instr) end, NewCtx};
    op_or -> {fun(Instr) -> LeftFun(Instr) orelse RightFun(Instr) end, NewCtx}
  end;
%%---
transform_pattern({{comparator, _, Operator}, LeftOperand, RightOperand}, Ctx) ->
  lager:info("Pattern operator \"~s\"", [Operator]),
  {LeftFun, LCtx} = transform_pattern(LeftOperand, Ctx),
  {RightFun, NewCtx} = transform_pattern(RightOperand, LCtx),
  case Operator of
    "=" -> {fun(Instr) -> LeftFun(Instr) == RightFun(Instr) end, NewCtx};
    ">" -> {fun(Instr) -> LeftFun(Instr) > RightFun(Instr) end, NewCtx};
    "<" -> {fun(Instr) -> LeftFun(Instr) < RightFun(Instr) end, NewCtx};
    ">=" -> {fun(Instr) -> LeftFun(Instr) >= RightFun(Instr) end, NewCtx};
    "<=" -> {fun(Instr) -> LeftFun(Instr) =< RightFun(Instr) end, NewCtx}
  end;
%%---
transform_pattern({{two_op_arith, _, Operator}, LeftOperand, RightOperand}, Ctx) ->
  lager:info("Pattern operator ~p", [Operator]),
  {LeftFun, LCtx} = transform_pattern(LeftOperand, Ctx),
  {RightFun, NewCtx} = transform_pattern(RightOperand, LCtx),
  case Operator of
    op_rem ->
      {
        fun(Instr) ->
          Left = LeftFun(Instr),
          Right = RightFun(Instr),
          Left - trunc(Left / Right) * Right
        end,
        NewCtx
      };
    op_plus -> {fun(Instr) -> LeftFun(Instr) + RightFun(Instr) end, NewCtx};
    op_minus -> {fun(Instr) -> LeftFun(Instr) - RightFun(Instr) end, NewCtx};
    op_multiply -> {fun(Instr) -> LeftFun(Instr) * RightFun(Instr) end, NewCtx};
    op_divide -> {fun(Instr) -> LeftFun(Instr) / RightFun(Instr) end, NewCtx}
  end;
%%---
transform_pattern({constant, _, Value}, Ctx) ->
  lager:info("Pattern operand CONST=~p", [Value]),
  {fun(_) -> Value end, Ctx};
%%---
transform_pattern({instr, _Line, {sma, SMAType, Text}}, Ctx) ->
  <<"Instr#", FrameType/binary>> = list_to_binary(Text),
  [Frame, _Rest] = binary:split(FrameType, <<"#">>),
  FrameName = proplists:get_value(Frame, rz_util:get_env(rz_server, pattern_names_to_frames)),
  CurStorageName = timeframe_worker:storage_name(FrameName),
  lager:info("Pattern operand get_SMA (~p:~p)", [FrameName, SMAType]),
  {
    fun(#candle{name = Instr}) ->
      case timeframe_worker:get_current_candle(Instr, CurStorageName) of
        {ok, #candle{smas = SMAs}} ->
          case lists:keyfind(SMAType, 1, SMAs) of
            {_, _, V} -> V;
            false -> throw(?NO_DATA)
          end;
        {error, not_found} -> throw(?NO_DATA)
      end
    end,
    Ctx};
%%---
transform_pattern({instr, Line, Instr}, Ctx) when is_list(Instr) ->
  transform_pattern({instr, Line, list_to_binary(Instr)}, Ctx);
transform_pattern({instr, Line, Instr}, Ctx) ->
  case binary:split(Instr, <<"#">>) of
    [<<"Instr">>, Data] -> transform_instr(Line, Data, ordinal_instr, Ctx);
    [?SNP_PREFIX, Data] -> transform_instr(Line, Data, snp_instr, Ctx)
  end.

%%--------------------------------------------------------------------
-spec transform_instr(Line :: non_neg_integer(), Data :: binary(), InstrType :: ordinal_instr | snp_instr, Ctx :: list()) -> {pattern_fun(), list()}.
transform_instr(Line, <<"Price">>, InstrType, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#PRICE">>, InstrType, Ctx);
%%---
transform_instr(Line, <<"Bid">>, InstrType, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#BID">>, InstrType, Ctx);
%%---
transform_instr(Line, <<"Ask">>, InstrType, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#ASK">>, InstrType, Ctx);
%%---
transform_instr(_, Data, InstrType, Ctx) ->
  [FrameOff, Val] = binary:split(Data, <<"#">>),
  [Frame, Offset] = binary:split(FrameOff, <<",">>),
  FrameName = proplists:get_value(Frame, rz_util:get_env(rz_server, pattern_names_to_frames)),
  CurStorageName = timeframe_worker:storage_name(FrameName),
  HistStorageName = online_history_worker:storage_name(FrameName),
  Length = proplists:get_value(history_depth, proplists:get_value(FrameName, rz_util:get_env(rz_server, frames))),
  {GetCandleFun, TempCtx} =
    case Offset of
      <<"1">> ->
        lager:info("Pattern operand get_current_candle(~p, Instr)", [CurStorageName]),
        UpdatedCtx = case proplists:get_value(use_current_candle, Ctx, undefined) of
                       true -> Ctx;
                       undefined -> Ctx ++ [{use_current_candle, true}]
                     end,
        {fun(#candle{name = Name}) -> timeframe_worker:get_current_candle(Name, CurStorageName)  end, UpdatedCtx};
      _ ->
        OffInt = binary_to_integer(Offset) - 2,
        lager:info("Pattern operand get_candle(~p, Instr, ~p, ~p)", [HistStorageName, Length, OffInt]),
        {
          fun(#candle{name = Name}) ->
            online_history_worker:get_candle(HistStorageName, Name, Length, OffInt)
          end,
          Ctx}
    end,
  NewCtx = update_referenced_frames(FrameName, TempCtx),
  ExtrFun =
    case Val of
      <<"OPEN">> -> fun(#candle{open = V}) -> V end;
      <<"CLOSE">> -> fun(#candle{close = V}) -> V end;
      <<"HIGH">> -> fun(#candle{high = V}) -> V end;
      <<"LOW">> -> fun(#candle{low = V}) -> V end;
      <<"PRICE">> -> fun(#candle{close = V}) -> V end;
      <<"BID">> -> fun(#candle{bid = V}) -> V end;
      <<"ASK">> -> fun(#candle{ask = V}) -> V end;
      <<"VOLUME">> -> fun(#candle{vol = V}) -> V end
    end,
  case InstrType of
    ordinal_instr ->
      {
        fun(Instr) ->
          case GetCandleFun(Instr) of
            {ok, C} -> ExtrFun(C);
            {error, not_found} -> throw(?NO_DATA)
          end
        end,
        NewCtx
      };
    snp_instr ->
      SNPName = rz_util:get_env(rz_server, snp_instr_name),
      SNPNameString = binary_to_list(SNPName),
      {
        fun(Name) when Name == SNPName orelse Name == SNPNameString ->
          case GetCandleFun(SNPName) of
            {ok, C} -> ExtrFun(C);
            {error, not_found} -> throw(?NO_DATA)
          end;
          (_) -> throw(?NO_DATA)
        end,
        NewCtx
      }
  end.

%%--------------------------------------------------------------------
update_referenced_frames(FrameName, Ctx) ->
  case proplists:get_value(referenced_frames, Ctx, []) of
    [] -> Ctx ++ [{referenced_frames, [FrameName]}];
    RefFrames ->
      case lists:member(FrameName, RefFrames) of
        true -> Ctx;
        false ->
          lists:keyreplace(referenced_frames, 1, Ctx, {referenced_frames, [FrameName | RefFrames]})
      end
  end.

%%--------------------------------------------------------------------
-spec transform_variables(tuple() | undefined) -> var_fun().
transform_variables(undefined) -> fun() -> [] end;
transform_variables(T) ->
  Vars = do_transform_vriables(T, []),
  fun() -> lists:map(F)