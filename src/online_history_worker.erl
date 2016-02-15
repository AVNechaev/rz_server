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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

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
