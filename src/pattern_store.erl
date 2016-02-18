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

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) -> {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
handle_info(_Info, _State) -> exit(handle_info_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
