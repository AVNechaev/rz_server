%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Feb 2016 19:11
%%%-------------------------------------------------------------------
-module(pat_exec_worker).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, load_pattern/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal.hrl").
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
-spec load_pattern(ThisPid :: pid(), PatDesc :: #pattern{}, PatFun :: pattern_fun()) -> ok.
load_pattern(ThisPid, PatDesc, PatFun) -> gen_server:call(ThisPid, {load_pattern, PatDesc, PatFun}).

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
handle_info(_Info, _State) -> exit(handle_info_unsupported).

%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
