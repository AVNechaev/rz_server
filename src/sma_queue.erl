%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2016 23:43
%%%-------------------------------------------------------------------
-module(sma_queue).
-author("anechaev").

%% API
-export([]).

-record(sma_q, {
  data :: tuple(),
  head :: integer(),
  tail :: integer(),
  size :: integer(),
  val :: float()
}).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
-spec new(Size :: integer()) -> {ok, #sma_q{}}.
new(Size) ->
  {ok, #sma_q{
    data = erlang:list_to_tuple([0 || _ <- lists:seq(1, Size)]),
    head = 0,
    tail = 0,
    size = Size,
    val = 0
  }}.

%%--------------------------------------------------------------------
cre_test() ->
  [
    ?assert(new(1) == {ok, #sma_q{data = {0}, head = 0, tail = 0, size = 1, val = 0}}),
    ?assert(new(3) == {ok, #sma_q{data = {0, 0, 0}, head = 0, tail = 0, size = 3, val = 0}}),
  ]