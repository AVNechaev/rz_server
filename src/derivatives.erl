%%%-------------------------------------------------------------------
%%% @author an
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2018 3:30 PM
%%%-------------------------------------------------------------------
-module(derivatives).
-author("an").

-include("internal.hrl").
%% API
-export([compute/1, is_derivative/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec is_derivative(InstrName :: instr_name()) -> boolean().
is_derivative(<<"USDX">>) -> true;
is_derivative(<<"USDY">>) -> true;
is_derivative(_) -> false.

%%--------------------------------------------------------------------
-spec compute(StorageName :: atom()) -> [#candle{}].
compute(Storage) ->
  lists:foldl(
    fun(F, Acc) ->
      try
        [F(Storage) | Acc]
      catch no_data ->
        Acc
      end
    end,
    [],
    [
      fun compute_usdx/1,
      fun compute_usdy/1
    ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec compute_usdx(StorageName :: atom()) -> #candle{}.
compute_usdx(StorageName) ->
  USDEUR = extract(<<"USDEUR.FXCM">>, StorageName),
  USDJPY = extract(<<"USDJPY.FXCM">>, StorageName),
  USDGBP = extract(<<"USDGBP.FXCM">>, StorageName),
  USDCAD = extract(<<"USDCAD.FXCM">>, StorageName),
  USDSEK = extract(<<"USDSEK.FXCM">>, StorageName),
  USDCHF = extract(<<"USDCHF.FXCM">>, StorageName),

  F = fun(Idx) ->
    50.14348112 *
      math:pow(erlang:element(Idx, USDEUR), 0.576) *
      math:pow(erlang:element(Idx, USDJPY), 0.136) *
      math:pow(erlang:element(Idx, USDGBP), 0.119) *
      math:pow(erlang:element(Idx, USDCAD), 0.091) *
      math:pow(erlang:element(Idx, USDSEK), 0.042) *
      math:pow(erlang:element(Idx, USDCHF), 0.036)
      end,
  #candle{
    name = <<"USDX">>,
    ask = F(#candle.ask),
    bid = F(#candle.bid),
    open = F(#candle.open),
    high = F(#candle.high),
    low = F(#candle.low),
    close = F(#candle.close),
    vol = trunc(F(#candle.vol))
  }.

compute_usdy(StorageName) ->
  USDCAD = extract(<<"USDCAD.FXCM">>, StorageName),
  USDSEK = extract(<<"USDSEK.FXCM">>, StorageName),
  USDCHF = extract(<<"USDCHF.FXCM">>, StorageName),

  F = fun(Idx) ->
    50.14348112 *
      math:pow(erlang:element(Idx, USDCAD), 0.091) *
      math:pow(erlang:element(Idx, USDSEK), 0.042) *
      math:pow(erlang:element(Idx, USDCHF), 0.036)
      end,
  #candle{
    name = <<"USDY">>,
    ask = F(#candle.ask),
    bid = F(#candle.bid),
    open = F(#candle.open),
    high = F(#candle.high),
    low = F(#candle.low),
    close = F(#candle.close),
    vol = trunc(F(#candle.vol))
  }.

%%--------------------------------------------------------------------
extract(InstrName, StorageName) ->
  case timeframe_worker:get_current_candle(InstrName, StorageName) of
    {ok, C} -> C;
    {error, not_found} -> throw(no_data)
  end.