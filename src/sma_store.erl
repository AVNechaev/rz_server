%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jul 2016 23:51
%%%-------------------------------------------------------------------
-module(sma_store).
-author("anechaev").

-behaviour(gen_server).

%% API
-export([start_link/0, add_daily_candle/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include_lib("rz_util/include/rz_util.hrl").

-define(SMA_KEY(Instr, Type), {sma, Instr, Type}). %% здесь хранятся последние посчитанные SMA
-define(HISTORY_DATA(Instr, Type), {hist, Instr, Type}). %% здесь хранятся свечки up to depth
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec add_daily_candle(#candle{}) -> ok.
add_daily_candle(Candle) -> gen_server:call(?SERVER, {add_daily_candle, Candle}).

%%--------------------------------------------------------------------
-spec get_sma(Instr :: instr_name(), Depth :: atom()).

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
