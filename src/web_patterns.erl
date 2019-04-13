%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Март 2016 18:37
%%%-------------------------------------------------------------------
-module(web_patterns).
-author("user").

%% API
-export([
  init/1,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  patterns_list/2,
  process_post/2, test/1]).

-include("internal.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(CANDLE_TEXT(TF, Num, Param), [<<"Instr#">>, TF, ",", Num, <<"#">>,Param]).

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['GET', 'POST'], ReqData, Context}.
content_types_provided(ReqData, Context) -> {[{"application/json", patterns_list}], ReqData, Context}.
content_types_accepted(ReqData, Context) -> {[{"application/json", process_post}], ReqData, Context}.

%%--------------------------------------------------------------------
patterns_list(ReqData, Context) ->
  {ok, Data} = pattern_store:get_patterns_indexes(),
  Payload = mochijson2:encode([{struct, [{id, Id}]} || Id <- Data]),
  {Payload, ReqData, Context}.

%%--------------------------------------------------------------------
process_post(ReqData, Context) ->
  try
    PatText = prepare_pattern_text(mochijson2:decode(wrq:req_body(ReqData))),
    case pattern_store:add_pattern(PatText) of
      {ok, #pattern{idx = Id}} ->
        lager:info("Created pattern ~p as ~p", [PatText, Id]),
        {{halt, 201}, wrq:set_resp_body(["{""id"":",integer_to_binary(Id), "}"], ReqData), Context};
      {error, Reason} ->
        lager:warning("HTTP handler warning:~p", [Reason]),
        {{halt, 400}, wrq:set_resp_body(<<"{""error"":""couldn't compile pattern""}">>, ReqData), Context}
    end
  catch
    M:E ->
      lager:warning("HTTP handler error: ~p:~p; ~p", [M, E, erlang:get_stacktrace()]),
      {{halt, 400}, wrq:set_resp_body(<<"{""error"":""couldn't parse request body""}">>, ReqData), Context}
  end.

%%--------------------------------------------------------------------
-spec prepare_pattern_text(Json :: mochijson2:json_term()) -> binary().
%% textual pattern
prepare_pattern_text({struct, [{<<"text">>, PatText}]}) -> PatText;
%% graph_patterns%
prepare_pattern_text({struct, Tokens}) ->
  Candles = proplists:get_value(<<"candles">>, Tokens),
  TF = proplists:get_value(<<"timeframe">>, Tokens),
  NumOfCandles = erlang:length(Candles),
%%  DO handles in history = Instr#D,n ... Instr#D,2
  [_FirstAND | Data] = lists:flatten(do_candles(NumOfCandles + 1, TF, Candles, [])),
  iolist_to_binary(
    [
      Data,
      <<" ">>,
      proplists:get_value(<<"variables">>, Tokens, <<>>)
    ]).


%%--------------------------------------------------------------------
do_candles(2, TF, [{struct, Candle}], Acc) -> Acc ++ [<<" AND ">>, candle_color(2, Candle, TF)];
do_candles(CurNum, TF, [{struct, CurCandle}, {struct, NextCandle} | Rest] = AllCandles, Acc) ->
  Data =
    [
      <<" AND ">>,
      check_left_right(TF, CurNum, <<"high">>, <<"HIGH">>, CurCandle, NextCandle),
      <<" AND ">>,
      check_left_right(TF, CurNum, <<"low">>, <<"LOW">>, CurCandle, NextCandle),
      <<" AND ">>,
      check_left_right(TF, CurNum, <<"close">>, <<"CLOSE">>, CurCandle, NextCandle),
      <<" AND ">>,
      candle_color(CurNum, CurCandle, TF),
      check_candle_over1(CurNum, TF, AllCandles)
    ],
  do_candles(CurNum - 1, TF, [{struct, NextCandle} | Rest], Acc ++ Data).

%%--------------------------------------------------------------------
%%check left & right candles 1-3-5-7, 2-4-6 etc
check_candle_over1(CurNum, TF, [{struct, CurCandle}, {struct, _}, {struct, Over1Candle} | _Rest]) ->
  [
    << " AND ">>,
    check_left_right(TF, CurNum, CurNum - 2, <<"high">>, <<"HIGH">>, CurCandle, Over1Candle),
    <<" AND ">>,
    check_left_right(TF, CurNum, CurNum - 2, <<"low">>, <<"LOW">>, CurCandle, Over1Candle)
  ];
%%---
check_candle_over1(_, _, _) -> [].

%%--------------------------------------------------------------------
check_left_right(TF, CurNum, Param, InstrParam, LeftHandle, RightCandle) ->
  check_left_right(TF, CurNum, CurNum - 1, Param, InstrParam, LeftHandle, RightCandle).

check_left_right(TF, CurNum, NextNum, Param, InstrParam, LeftHandle, RightCandle) ->
  LVal = proplists:get_value(Param, LeftHandle),
  RVal = proplists:get_value(Param, RightCandle),
  CNumBin = integer_to_binary(CurNum),
  NNumBin = integer_to_binary(NextNum),
  [
    ?CANDLE_TEXT(TF, CNumBin, InstrParam),
    g_or_l(LVal, RVal),
    ?CANDLE_TEXT(TF, NNumBin, InstrParam)
  ].

%%--------------------------------------------------------------------
candle_color(CurNum, CandleJson, TF) ->
  Open = proplists:get_value(<<"open">>, CandleJson),
  Close = proplists:get_value(<<"close">>, CandleJson),
  CurNumBin = integer_to_binary(CurNum),
  [
    ?CANDLE_TEXT(TF, CurNumBin, <<"OPEN">>),
    case Open of
      _ when Open > Close -> <<">">>;
      _ when Open == Close -> <<"=">>;
      _ -> "<"
    end,
    ?CANDLE_TEXT(TF, CurNumBin, <<"CLOSE">>)
  ].

%%--------------------------------------------------------------------
g_or_l(N1, N2) when N1 > N2 -> <<">">>;
g_or_l(N1, N2) when N1 == N2 -> <<"=">>;
g_or_l(N1, N2) when N1 < N2 -> <<"<">>.

test(Filename) ->
  {ok, D} = file:read_file(Filename),
  prepare_pattern_text(mochijson2:decode(D)).
