%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Февр. 2016 12:00
%%%-------------------------------------------------------------------
-module(pattern_store).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/0, add_pattern/1, delete_all_patterns/0, remove_pattern/1, get_patterns_indexes/0, replace_pattern/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal.hrl").
-define(SERVER, ?MODULE).

-type frame_desc() :: {Name :: atom(), Duration :: non_neg_integer() | undefined}.
-record(state, {frames_list :: [frame_desc()]}).

-record(add_res, {id :: non_neg_integer()}).
-record(sel_res, {id :: non_neg_integer(), expr :: binary() | list()}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  {ok, Ret} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  gen_server:call(?SERVER, init_patterns),
  {ok, Ret}.

%%--------------------------------------------------------------------
-spec add_pattern(Pattern :: pattern_text()) -> {ok, #pattern{}} | {error, Reason :: term()}.
add_pattern(Pattern) -> gen_server:call(?SERVER, {add_pattern, Pattern}).

%%--------------------------------------------------------------------
-spec delete_all_patterns() -> ok.
delete_all_patterns() -> gen_server:call(?SERVER, delete_all_patterns).

%%--------------------------------------------------------------------
-spec replace_pattern(Id :: pattern_index(), NewText :: pattern_text) -> ok | {error, not_found | {compile_error, Reason :: term()}}.
replace_pattern(Id, NewText) -> gen_server:call(?SERVER, {replace_pattern, Id, NewText}).

%%--------------------------------------------------------------------
-spec remove_pattern(Id :: pattern_index()) -> ok | {error, not_found}.
remove_pattern(Id) -> gen_server:call(?SERVER, {remove_pattern, Id}).

%%--------------------------------------------------------------------
-spec get_patterns_indexes() -> {ok, [pattern_index()]}.
get_patterns_indexes() -> gen_server:call(?SERVER, get_patterns_indexes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  SortedByDuration = lists:sort(
    fun({_N1, D1}, {_N2, D2}) -> D1 < D2 end,
    [{Name, proplists:get_value(duration, Params)} || {Name, Params} <- rz_util:get_env(rz_server, frames)]
  ),
  {ok, #state{frames_list = SortedByDuration}}.

%%--------------------------------------------------------------------
handle_call({add_pattern, PatternText}, _From, State) ->
  [_, ResPacket] = emysql:execute(mysql_config_store, <<"insert into PATTERNS (expr) VALUES ('", PatternText/binary, "'); select LAST_INSERT_ID() as id">>),
  [#add_res{id = Id}] = emysql:as_record(ResPacket, add_res, record_info(fields, add_res)),
  lager:info("STORE pattern: ~p AS ~p", [PatternText, Id]),
  Pattern = pt_to_pattern(Id, PatternText),
  case patterns_executor:load_pattern(Pattern) of
    {ok, RefFrames} ->
      activate_pattern(RefFrames, State#state.frames_list),
      {reply, {ok, Pattern}, State};
    {error, What} ->
      lager:warning("An error occured when adding a pattern: ~p", [What]),
      IdBin = erlang:integer_to_binary(Id),
      emysql:execute(mysql_config_store, <<"delete from PATTERNS where id=",IdBin/binary>>),
      {reply, {error, {compile_error, What}}, State}
  end;

%%---
handle_call(delete_all_patterns, _From, State) ->
  [
    patterns_executor:delete_pattern(Id)
    || #add_res{id = Id} <-
    emysql:as_record(
      emysql:execute(mysql_config_store, <<"select id from PATTERNS">>),
      add_res,
      record_info(fields, add_res))
  ],
  emysql:execute(mysql_config_store, <<"delete from PATTERNS">>),
  lager:warning("All patterns deleted"),
  {reply, ok, State};
%%---
handle_call({replace_pattern, Id, NewText}, _From, State) ->
  lager:info("REPLACING pattern # ~p with ~p", [Id, NewText]),
  IdBin = integer_to_binary(Id),
  Pattern = pt_to_pattern(Id, NewText),
  case emysql:as_record(
    emysql:execute(mysql_config_store, <<"select id from PATTERNS WHERE id=", IdBin/binary>>),
    add_res,
    record_info(fields, add_res)) of
    [] -> {reply, {error, not_found}, State};
    _ ->
      patterns_executor:delete_pattern(Id),
      case patterns_executor:load_pattern(Pattern) of
        {ok, RefFrames} ->
          emysql:execute(
            mysql_config_store,
            <<"update PATTERNS set expr='", NewText/binary,"' where ID=", IdBin/binary>>),
          activate_pattern(RefFrames, State#state.frames_list),
          {reply, ok, State};
        {error, What} ->
          lager:warning("An error occured when adding a pattern: ~p", [What]),
          {reply, {error, {compile_error, What}}, State}
      end
  end;
%%---
handle_call({remove_pattern, Id}, _From, State) ->
  IdText = integer_to_binary(Id),
  case emysql:as_record(
    emysql:execute(mysql_config_store, <<"select id from PATTERNS WHERE id=", IdText/binary>>),
    add_res,
    record_info(fields, add_res)) of
    [] -> {reply, {error, not_found}, State};
    _ ->
      patterns_executor:delete_pattern(Id),
      emysql:execute(mysql_config_store, <<"delete from PATTERNS WHERE id=", IdText/binary>>),
      lager:info("Delete pattern #~p", [Id]),
      {reply, ok, State}
  end;
%%---
handle_call(init_patterns, _From, State) ->
  [
    {ok, _} = patterns_executor:load_pattern(pt_to_pattern(Id, Text))
    || #sel_res{id = Id, expr = Text} <-
    emysql:as_record(
      emysql:execute(mysql_config_store, <<"select id, expr from PATTERNS">>),
      sel_res,
      record_info(fields, sel_res))
  ],
  {reply, ok, State};
%%---
handle_call(get_patterns_indexes, _From, State) ->
  Ret = [
    Id
    || #sel_res{id = Id} <-
    emysql:as_record(
      emysql:execute(mysql_config_store, <<"select id, expr from PATTERNS">>),
      sel_res,
      record_info(fields, sel_res))
  ],
  {reply, {ok, Ret}, State}.

%%--------------------------------------------------------------------
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
handle_info(_Info, _State) -> exit(handle_info_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
pt_to_pattern(Idx, Text) when is_list(Text) -> pt_to_pattern(Idx, list_to_binary(Text));
pt_to_pattern(Idx, Text) when is_binary(Text) ->
  #pattern{
    idx = Idx,
    text = Text,
    md5 = erlang:md5(Text)
  }.

%%--------------------------------------------------------------------
-spec activate_pattern(RefFrames :: [atom()], FramesList :: [frame_desc()]) -> ok.
activate_pattern(ReferencedFrames, FramesList) ->
  %%  activation goes here:
  [MinDurationFrame | _] = [Name || {Name, _} <- FramesList, RefName <- ReferencedFrames, Name == RefName],
  timeframe_worker:activate_fires(timeframe_worker:reg_name(MinDurationFrame)).
