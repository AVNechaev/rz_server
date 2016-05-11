%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Feb 2016 20:08
%%%-------------------------------------------------------------------
-module(online_history_worker).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/3, reg_name/1, add_recent_candle/2, storage_name/1, get_candle/4, set_instrs/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").
-include("internal.hrl").

-type buf_counter_name_t() :: {counter, instr_name()}.

-record(state, {
  name :: atom(),
  storage :: ets_limbuffer:storage_t(),
  depth :: non_neg_integer(),
  buffer_on_the_fly :: boolean(),
  known_buffers :: dict:dict(instr_name(), buf_counter_name_t()) | undefined
}).

-compile([{parse_transform, lager_transform}]).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(Name :: atom(), Params :: list(), Instrs :: list()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Params, Instrs) -> gen_server:start_link({local, reg_name(Name)}, ?MODULE, [Name, Params, Instrs], []).

%%--------------------------------------------------------------------
-spec reg_name(Name :: atom()) -> atom().
reg_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_online_history_worker").

%%--------------------------------------------------------------------
-spec storage_name(Name :: atom()) -> atom().
storage_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_ets_history_buffer").

%%--------------------------------------------------------------------
-spec add_recent_candle(ThisName :: atom(), Candle :: #candle{}) -> ok.
add_recent_candle(ThisName, Candle) -> gen_server:cast(ThisName, {add_recent_candle, Candle}).

%%--------------------------------------------------------------------
-spec get_candle(StorageName :: atom(), Instr :: instr_name(), Length :: pos_integer(), Depth :: pos_integer()) -> {ok, #candle{}} | {error, not_found}.
get_candle(StorageName, Instr, Length, Depth) ->
  ets_limbuffer:get(StorageName, Instr, buf_counter_name(Instr), Length, Depth).

%%--------------------------------------------------------------------
-spec set_instrs(ThisName :: atom(), Instrs :: [instr_name()]) -> ok.
set_instrs(ThisName, Instrs) -> gen_server:call(ThisName, {set_instrs, Instrs}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Params, Instrs]) ->
  UniqInstrs = lists:usort(Instrs),
  Storage = ets_limbuffer:create_storage(storage_name(Name)),
  Depth = proplists:get_value(history_depth, Params),
  Buffers = case proplists:get_value(buffers_on_the_fly, Params) of
              true -> dict:new();
              false ->
                lists:foreach(fun(I) -> ets_limbuffer:create_buffer(Storage, I, buf_counter_name(I), Depth) end, UniqInstrs),
                undefined
            end,
  {ok, #state{
    name = Name,
    storage = Storage,
    depth = Depth,
    buffer_on_the_fly = proplists:get_value(buffers_on_the_fly, Params),
    known_buffers = Buffers}}.

%%--------------------------------------------------------------------
handle_cast({add_recent_candle, Candle = #candle{name = Instr}}, State = #state{buffer_on_the_fly = true, known_buffers = Buffers}) ->
  CntName = buf_counter_name(Instr),
  NewBuff = case dict:find(Instr, Buffers) of
              error ->
                ets_limbuffer:create_buffer(State#state.storage, Instr, CntName, State#state.depth),
                dict:store(Instr, CntName, Buffers);
              {ok, _} ->
                Buffers
            end,
  ets_limbuffer:push(Candle, State#state.storage, Instr, CntName, State#state.depth),
  {noreply, State#state{known_buffers = NewBuff}};
%%---
%% при перезагрузке списка инструментов может быть, что в mailbox остался старый тик
%% с именем, которого уже нет, поэтому будем засовывать данные в буфер, забивая на взможные ошибки
handle_cast({add_recent_candle, Candle = #candle{name = Instr}}, State) ->
  ets_limbuffer:maybe_push(Candle, State#state.storage, Instr, buf_counter_name(Instr), State#state.depth),
  {noreply, State}.

%%--------------------------------------------------------------------
handle_call({set_instrs, _}, _From, State = #state{buffer_on_the_fly = true}) ->
  ok = ets_limbuffer:delete_buffers(State#state.storage),
  {reply, ok, State};
%%---
handle_call({set_instrs, Instrs}, _From, State) ->
  UniqInstrs = lists:usort(Instrs),
  ok = ets_limbuffer:delete_buffers(State#state.storage),
  lists:foreach(
    fun(I) ->
      ets_limbuffer:create_buffer(State#state.storage, I, buf_counter_name(I), State#state.depth)
    end,
    UniqInstrs),
  {reply, ok, State}.

%%--------------------------------------------------------------------
handle_info(_Info, _State) -> exit(handle_info_unsupported).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec buf_counter_name(Instr :: instr_name()) -> buf_counter_name_t().
buf_counter_name(Instr) -> {counter, Instr}.