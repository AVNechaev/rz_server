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
-export([create_storage/0, create_buffer/3, push/2]).
-type(storage_t() :: ets:tid()).

-record(context, {
  tid :: storage_t(),
  counter_name :: atom(),
  name :: atom(),
  length :: pos_integer()
}).

-type context() :: #context{}.
-export_type([context/0]).

%%%-------------------------------------------------------------------
-spec create_storage() -> storage_t().
create_storage() -> ets:new(limbuffer, [set, protected, {read_concurrency, true}]).

%%%-------------------------------------------------------------------
-spec create_buffer(Tid :: storage_t(), Name :: atom(), Length :: pos_integer()) -> {ok, #context{}}.
create_buffer(Tid, Name, Length) ->
  Counter = list_to_atom(atom_to_list(Name ++ "_counter")),
  true = ets:insert_new(Tid, {Counter, -1}), % default value; will be 0 at the 1st push
  {ok, #context{counter_name = Counter, name = Name, length = Length, tid = Tid}}.

%%%-------------------------------------------------------------------
-spec push(Data :: any(), Buffer :: #context) -> ok.
push(Data, Context = #context{tid = Tid, counter_name = Cnt}) ->
  ets:update_counter(Tid, Cnt, {2, 1, Context#context.length, 0}).
  hui.
