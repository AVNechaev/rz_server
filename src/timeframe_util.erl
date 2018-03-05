%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Окт. 2016 17:23
%%%-------------------------------------------------------------------
-module(timeframe_util).
-author("user").

%% API
-export([candle_to_json/2, populate_sma_queues/5]).

-include("internal.hrl").
-include_lib("rz_util/include/rz_util.hrl").

-record(last_price_rec, {
  open :: float(),
  close :: float(),
  high :: float(),
  low :: float(),
  vol :: integer()}).

-compile([{parse_transform, lager_transform}]).

%%--------------------------------------------------------------------
-spec candle_to_json(#candle{}, CandleStartBin :: binary()) -> iolist().
candle_to_json(#candle{name = N, open = O, high = H, low = L, close = C, vol = V, smas = SMAs}, CandleStartBin) ->
  {Mega, Sec, Micro} = erlang:timestamp(),
  TSB = integer_to_binary(Micro + Sec * 1000000 + Mega * 1000000000000),
  OB = float_to_binary(O, [{decimals, 4}]),
  HB = float_to_binary(H, [{decimals, 4}]),
  LB = float_to_binary(L, [{decimals, 4}]),
  CB = float_to_binary(C, [{decimals, 4}]),
  VB = integer_to_binary(V),

  SMAText =
    [
      [",\"", T, "\":\"", float_to_binary(SV, [{decimals, 4}]), "\""]
      || {_, T, SV} <- SMAs
    ],
  [
    "{\"timestamp\":",
    TSB,
    ",",
    "\"data\":{",
    "\"name\":",
    "\"",
    N,
    "\",",
    "\"ts\":",
    "\"",
    CandleStartBin,
    "\",",
    "\"open\":",
    "\"",
    OB,
    "\",",
    "\"high\":",
    "\"",
    HB,
    "\",",
    "\"low\":",
    "\"",
    LB,
    "\",",
    "\"close\":",
    "\"",
    CB,
    "\",",
    "\"volume\":",
    "\"",
    VB,
    "\"",
    SMAText,
    "}}"
  ].

%%--------------------------------------------------------------------
-spec populate_sma_queues(
    HistName :: atom(),
    Instr :: instr_name(),
    TableName :: binary(),
    MaxDepth :: integer(),
    SMAs :: [{term()}]) -> [{SMAName :: atom(), Q :: #sma_q{}}].
populate_sma_queues(HistName, Instr, TableName, MaxDepth, SMAs) ->
  lager:info("Filling the sma queue [~p:~p]", [Instr, TableName]),
  SQL = [
    "SELECT open,close,high,low,volume as vol FROM ",
    TableName,
    " WHERE name='",
    Instr, "' ",
    "ORDER BY ts DESC LIMIT ",
    integer_to_binary(MaxDepth)
  ],
  Data = lists:reverse(
    emysql:as_record(
      emysql:execute(mysql_candles_store, SQL),
      last_price_rec,
      record_info(fields, last_price_rec))),
  ResQ =
    lists:foldl(
      fun(Rec = #last_price_rec{close = V}, Queues) ->
        SMAVals = [{Name, BinName, sma_queue:probe_sma(V, Q)} || {Q, Name, BinName} <- Queues],
        Candle = #candle{
          name = Instr,
          open = Rec#last_price_rec.open,
          close = V,
          high = Rec#last_price_rec.high,
          low = Rec#last_price_rec.low,
          vol = Rec#last_price_rec.vol,
          smas = SMAVals
        },
        online_history_worker:add_recent_candle(HistName, Candle),
        [{sma_queue:store(V, Q), Name, BinName} || {Q, Name, BinName} <- Queues]
      end,
      [{sma_queue:new(Depth), Name, BinName} || {Name, BinName, Depth} <- SMAs],
      Data
    ),
  lists:zip([N || {N, _, _} <- SMAs], [Q || {Q, _, _} <- ResQ]).