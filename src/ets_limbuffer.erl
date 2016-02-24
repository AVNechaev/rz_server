%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Single-writer multiple-reader limited length stack with direct access
%%% to elements by index; index 0 means the last inserted element;
%%% index length - 1 means the oldest element;
%%% @end
%%% Created : 14. Feb 2016 20:11
%%%-------------------------------------------------------------------
-module(ets_limbuffer).
-author("anechaev").

%% API
-export([create_storage/0, create_buffer/3, push/2, get/2, create_storage/1, desired_counter_name/1, create_buffer/4, push/5, get/5]).
-type storage_t() :: ets:tid().
-type buffer_name() :: binary() | list() | atom().

-record(context, {
  tid :: storage_t(),
  counter_name :: atom(),
  name :: buffer_name(),
  length :: pos_integer()
}).

-type context() :: #context{}.
-define(VAL_KEY(Name, Index), {Name, Index}).
-export_type([context/0, storage_t/0]).

%%%-------------------------------------------------------------------
-spec create_storage() -> storage_t().
create_storage() -> ets:new(limbuffer, [set, protected, {read_concurrency, true}]).

%%%-------------------------------------------------------------------
-spec create_storage(StorageName :: atom()) -> storage_t().
create_storage(StorageName) -> ets:new(StorageName, [set, protected, {read_concurrency, true}, named_table]).

%%%-------------------------------------------------------------------
-spec desired_counter_name(Name :: buffer_name()) -> atom().
desired_counter_name(Name) -> list_to_atom(any_to_list(Name) ++ "_counter").

%%%-------------------------------------------------------------------
-spec create_buffer(Tid :: storage_t(), Name :: buffer_name(), Length :: pos_integer()) -> {ok, #context{}}.
create_buffer(Tid, Name, Length) ->
  Counter = desired_counter_name(Name),
  create_buffer(Tid, Name, Counter, Length),
  {ok, #context{counter_name = Counter, name = Name, length = Length, tid = Tid}}.

%%%-------------------------------------------------------------------
-spec create_buffer(Tid :: storage_t(), Name :: buffer_name(), CounterName :: any(), Length :: pos_integer()) -> ok.
create_buffer(Tid, Name, CounterName, Length) ->
  [ets:insert(Tid, {?VAL_KEY(Name, I), undefined}) || I <- lists:seq(0, Length - 1)],
  true = ets:insert_new(Tid, {CounterName, -1}), % default value; will be 0 at the 1st push
  ok.

%%%-------------------------------------------------------------------
-spec push(Data :: any(), Buffer :: #context{}) -> ok.
push(Data, #context{tid = Tid, counter_name = Counter, name = Name, length = Length}) ->
  push(Data, Tid, Name, Counter, Length).

%%%-------------------------------------------------------------------
-spec push(Data :: any(), Tid :: storage_t(), Name :: buffer_name(), CounterName :: any(), Length :: pos_integer()) -> ok.
push(Data, Tid, Name, CounterName, Length) ->
  [{_, V}] = ets:lookup(Tid, CounterName),
  Id = (V + Length + 1) rem Length,
  ets:insert(Tid, {?VAL_KEY(Name, Id), Data}),
  Id = ets:update_counter(Tid, CounterName, {2, 1, Length - 1, 0}),
  ok.

%%%-------------------------------------------------------------------
%depth - 0..length -1
-spec get(Buffer :: #context{}, Depth :: non_neg_integer()) -> {ok, Data :: any()} | {error, not_found}.
get(#context{length = Length}, Depth) when Depth >= Length -> {error, not_found};
get(#context{tid = Tid, name = Name, counter_name = CounterName, length = Length}, Depth) ->
  get(Tid, Name, CounterName, Length, Depth).

%%%-------------------------------------------------------------------
-spec get(Tid :: storage_t(), Name :: buffer_name(), CounterName :: any(), Length :: pos_integer(), Depth :: non_neg_integer()) -> {ok, Data :: any()} | {error, not_found}.
get(Tid, Name, CounterName, Length, Depth) ->
  case ets:lookup(Tid, CounterName) of
    [] -> {error, not_found};
    [{_, V}] ->
      Id = (V + Length - Depth) rem Length,
      case ets:lookup(Tid, ?VAL_KEY(Name, Id)) of
        [{_, undefined}] -> {error, not_found};
        [{_, Data}] -> {ok, Data}
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
any_to_list(D) when is_list(D) -> D;
any_to_list(D) when is_binary(D) -> binary_to_list(D);
any_to_list(D) when is_atom(D) -> atom_to_list(D).