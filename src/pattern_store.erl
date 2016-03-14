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
-export([start_link/0, add_pattern/1, delete_all_patterns/0, remove_pattern/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal.hrl").
-define(SERVER, ?MODULE).

-record(state, {}).

-define(ADD_STMT, list_to_atom(atom_to_list(?MODULE) ++ "_add_stmt")).
-record(add_res, {id :: non_neg_integer()}).
-record(sel_res, {id :: non_neg_integer, text :: binary() | list()}).

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
-spec add_pattern(Pattern :: pattern_text()) -> #pattern{}.
add_pattern(Pattern) -> gen_server:call(?SERVER, {add_pattern, Pattern}).

%%--------------------------------------------------------------------
-spec delete_all_patterns() -> ok.
delete_all_patterns() -> gen_server:call(?SERVER, delete_all_patterns).

%%--------------------------------------------------------------------
-spec remove_pattern(Id :: pattern_index()) -> ok | {error, not_found}.
remove_pattern(Id) -> gen_server:call(?SERVER, {remove_pattern, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  emysql:prepare(?ADD_STMT, <<"insert into PATTERNS (expr) VALUES (?); select LAST_INSERT_ID()">>),
  {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call({add_pattern, PatternText}, _From, State) ->
  [#add_res{id = Id}] = emysql:as_record(
    emysql:execute(mysql_config_store, ?ADD_STMT, [PatternText]),
    add_res,
    record_info(fields, add_res)
  ),
  lager:info("STORE pattern: ~p AS ~p", [PatternText, Id]),
  {reply, pt_to_pattern(Id, PatternText), State};
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
handle_call({remove_pattern, Id}, _From, State) ->
  IdText = integer_to_binary(Id),
  emysql:execute(mysql_config_store, <<"delete from PATTERNS WHERE id=", IdText/binary>>),
  lager:info("Delete pattern #~p", [Id]),
%%   TODO: impl {error, not_found} case
  {reply, ok, State};
%%---
handle_call(init_patterns, _From, State) ->
  [
    patterns_executor:load_pattern(pt_to_pattern(Id, Text))
    || #sel_res{id = Id, text = Text} <-
    emysql:as_record(
      emysql:execute(mysql_config_store, <<"select id, expr from PATTERNS">>),
      sel_res,
      record_info(fields, sel_res))
  ],
  {reply, ok, State}.

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