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
  act_size :: integer(),
  val :: float()
}).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
-spec new(Size :: integer()) -> #sma_q{}.
new(Size) ->
  #sma_q{
    data = erlang:list_to_tuple([0 || _ <- lists:seq(1, Size)]),
    head = 0,
    tail = 0,
    size = Size,
    act_size = 0,
    val = 0
  }.

%%--------------------------------------------------------------------
-spec store(V :: integer() | float(), Q :: #sma_q{}) -> #sma_q{}.
store(V, Q = #sma_q{head = H, tail = T, val = SMA, data = D, act_size = S, size = S}) ->
  Last = erlang:element(T + 1, D),
  ND = erlang:setelement(H + 1, D, V),
  NH = (H + 1) rem S,
  NT = (T + 1) rem S,
  NSMA = SMA + (V - Last) / S,
  Q#sma_q{data = ND, head = NH, tail = NT, val = NSMA};
%%---
store(V, Q = #sma_q{head = H, val = SMA, data = D, act_size = S}) ->
  ND = erlang:setelement(H + 1, D, V),
  NH = (H + 1) rem Q#sma_q.size,
  NSMA = (SMA * S + V) / (S + 1),
  Q#sma_q{data = ND, head = NH, val = NSMA, act_size = S + 1}.

%%--------------------------------------------------------------------
eq(V1, V2) -> abs(V1 - V2) < 0.001.

cre_test() ->
  [
    ?assert(new(1) == #sma_q{data = {0}, head = 0, tail = 0, size = 1, val = 0, act_size = 0}),
    ?assert(new(3) == #sma_q{data = {0, 0, 0}, head = 0, tail = 0, size = 3, val = 0, act_size = 0})
  ].

store_test() ->
  Q1 = store(1, new(3)),
  ?assert(Q1#sma_q.data == {1, 0, 0}),
  ?assert(eq(Q1#sma_q.val, 1)),

  Q2 = store(2, Q1),
  ?assert(Q2#sma_q.data == {1, 2, 0}),
  ?assert(eq(Q2#sma_q.val, 1.5)),

  Q3 = store(3, Q2),
  ?assert(Q3#sma_q.data == {1, 2, 3}),
  ?assert(eq(Q3#sma_q.val, 2)),

  Q4 = store(4, Q3),
  ?assert(Q4#sma_q.data == {4, 2, 3}),
  ?assert(eq(Q4#sma_q.val, 3)),

  Q5 = store(5, Q4),
  ?assert(Q5#sma_q.data == {4, 5, 3}),
  ?assert(eq(Q5#sma_q.val, 4)).
