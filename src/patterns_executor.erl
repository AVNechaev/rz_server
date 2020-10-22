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
-export([start_link/0, load_pattern/1, check_patterns/2, delete_pattern/1, compile_pattern/1, load_timecodes_data/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("internal.hrl").

-type logic_fun() :: fun((instr_name()) -> boolean()).
-type number_fun() :: fun((instr_name()) -> number()).

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
-spec load_pattern(Pat :: #pattern{}) -> {ok, RefFrames :: [atom()]} | {error, Reason :: term()}.
load_pattern(Pat) -> gen_server:call(?SERVER, {load_pattern, Pat}).

%%--------------------------------------------------------------------
-spec delete_pattern(pattern_index()) -> ok.
delete_pattern(PatId) -> gen_server:call(?SERVER, {delete_pattern, PatId}).

%%--------------------------------------------------------------------
%% см timeframe_worker:universal_to_candle_time()
-spec check_patterns(Instr :: pattern_fun_arg(), UniversalCandleTime :: pos_integer()) -> ok.
check_patterns(Instr, UniversalCandleTime) -> gen_server:cast(?SERVER, {check_patterns, Instr, UniversalCandleTime}).

%%--------------------------------------------------------------------
-spec load_timecodes_data(TCData :: [pat_exec_worker:timecode()]) -> ok.
load_timecodes_data(TCData) -> gen_server:call(?SERVER, {load_timecodes_data, TCData}).

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
    {ok, TimeCode, {Fun, Ctx}, VarFun} = compile_pattern(Pat),
    lager:info("Pattern compiled; Context: ~p", [Ctx]),
    ReferencedFrames = proplists:get_value(referenced_frames, Ctx, []),
    UsingCurrentCandle =
      case proplists:get_value(use_current_candle, Ctx, false) of
        false -> false;
        true ->
          case proplists:get_value(enable_current_candle, rz_util:get_env(rz_server, patterns_executor), false) of
            false ->
              lager:info("Force disable using current candle!"),
              false;
            true -> true
          end
      end,
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
    TimeCodedFun =
      fun(FireData, TimecodeFun) ->
        case TimecodeFun(TimeCode) of
          true ->
            AnchoredFun(FireData);
          false ->
            false
        end
      end,
    pat_exec_worker:load_pattern(elect(State), Pat, {TimeCodedFun, VarFun}),

    {reply, {ok, ReferencedFrames}, State}
  catch
    M:E ->
      {reply, {error, {M, E, erlang:get_stacktrace()}}, State}
  end;
%%---
handle_call({delete_pattern, PatId}, _From, State) ->
  lager:info("Deleting pattern ~p", [PatId]),
  [pat_exec_worker:delete_pattern(Pid, PatId) || Pid <- State#state.workers],
  {reply, ok, State};
%%---
handle_call({load_timecodes_data, TCData}, _From, State) ->
  lager:info("Loading new Timecodes data..."),
  [pat_exec_worker:load_timecodes_data(Pid, TCData) || Pid <- State#state.workers],
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
elect(#state{workers = Workers}) -> lists:nth(rand:uniform(length(Workers)), Workers).

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
  {ok, {TimeCode, ParsedPattern, ParsedVariables}} = patterns_parser:parse(Tokens),
  {ok, extract_timecode(TimeCode), transform_pattern(ParsedPattern, []), transform_variables(ParsedVariables)}.

%%--------------------------------------------------------------------
-spec transform_pattern(tuple(), list()) -> {logic_fun() | number_fun(), list()}.
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
    fun(Instr) ->
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
transform_pattern({instr, Line, <<"Instr#", Data/binary>>}, Ctx) -> transform_instr(Line, Data, undefined, Ctx);
%%---
transform_pattern({fixed_instr, Line, Instr}, Ctx) when is_list(Instr) ->
  transform_pattern({fixed_instr, Line, list_to_binary(Instr)}, Ctx);
transform_pattern({fixed_instr, Line, <<"FIXED_", Rest/binary>>}, Ctx) ->
  [FixedInstr, Data] = binary:split(Rest, <<"#">>),
  transform_instr(Line, Data, FixedInstr, Ctx).


%%--------------------------------------------------------------------
-spec transform_instr(Line :: non_neg_integer(), Data :: binary(), FixedInstr :: instr_name() | undefined, Ctx :: list()) -> {number_fun(), list()}.
transform_instr(Line, <<"Price">>, FixedInstr, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#PRICE">>, FixedInstr, Ctx);
%%---
transform_instr(Line, <<"Bid">>, FixedInstr, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#BID">>, FixedInstr, Ctx);
%%---
transform_instr(Line, <<"Ask">>, FixedInstr, Ctx) ->
  DefFrame = proplists:get_value(frame_for_current_candle, rz_util:get_env(rz_server, patterns_executor)),
  transform_instr(Line, <<DefFrame/binary, ",1#ASK">>, FixedInstr, Ctx);
%%---
transform_instr(_, Data, FixedInstr, Ctx) ->
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
        case FixedInstr of
          undefined ->
            {fun(Name) -> timeframe_worker:get_current_candle(Name, CurStorageName) end, UpdatedCtx};
          _ ->
            {fun(_Name) -> timeframe_worker:get_current_candle(FixedInstr, CurStorageName) end, UpdatedCtx}
        end;
      _ ->
        OffInt = binary_to_integer(Offset) - 2,
        lager:info("Pattern operand get_candle(~p, Instr, ~p, ~p)", [HistStorageName, Length, OffInt]),
        case FixedInstr of
          undefined ->
            {
              fun(Name) ->
                online_history_worker:get_candle(HistStorageName, Name, Length, OffInt)
              end,
              Ctx
            };
          _ ->
            {
              fun(_Name) ->
                online_history_worker:get_candle(HistStorageName, FixedInstr, Length, OffInt)
              end,
              Ctx
            }
        end
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
  {
    fun(Instr) ->
      case GetCandleFun(Instr) of
        {ok, C} ->
          case ExtrFun(C) of
            Num when is_number(Num) ->
              case abs(Num) of
                Lower when Lower < 0.001 ->
                  lager:warning("Zero value detected: [~p; ~p; ~p]: ~p", [HistStorageName, Instr, Val, Lower]),
                  throw(?NO_DATA);
                Normal -> Normal
              end;
            _ -> throw(?NO_DATA)
          end;
        {error, not_found} -> throw(?NO_DATA)
      end
    end,
    NewCtx
  }.

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
transform_variables(undefined) -> fun(_Candle) -> [] end;
transform_variables(T) ->
  Vars = do_transform_variables(T, []),
  fun(Instr) ->
    lists:map(
      fun({Name, ExtractF}) ->
        try
          {Name, ExtractF(Instr)}
        catch
          ?NO_DATA ->
            {Name, undefined}
        end
      end,
      Vars)
  end.
%%--------------------------------------------------------------------

do_transform_variables(undefined, Acc) -> lists:reverse(Acc);
do_transform_variables({{variable, _, Name}, VarData, Rest}, Acc) ->
  {F, _} = transform_pattern(VarData, []),
  do_transform_variables(Rest, [{Name, F} | Acc]).

%%--------------------------------------------------------------------
extract_timecode(undefined) -> undefined;
extract_timecode({time_code, TokenLine, TCData}) when is_list(TCData) -> extract_timecode({time_code, TokenLine, list_to_binary(TCData)});
extract_timecode({time_code, _, <<"TC:=", TC/binary>>}) -> {time_code, TC}.