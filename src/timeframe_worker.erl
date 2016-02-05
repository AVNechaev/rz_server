%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Февр. 2016 19:18
%%%-------------------------------------------------------------------
-module(timeframe_worker).
-author("user").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

-record(candle, {
  open :: float(),
  close :: float(),
  high :: float(),
  low :: float(),
  vol :: float()
}).

-record(state, {
  enabled :: boolean(),
  date :: calendar:date(),
  begin_time :: calendar:time(),
  tid :: ets:tid(),
  duration :: pos_integer()
}).

-type frame_params() :: list().

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(Name :: atom(), Params :: frame_params()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Params) ->
  gen_server:start_link({localp, Name}, ?MODULE, [Params], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Params]) ->
  Tid = ets:new(frame_candles, [private, set]),
  {ok,
    #state{
      tid = Tid,
      duration = proplists:get_value(duration, Params),
      enabled = false
    }}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
handle_info(_Info, _State) -> exit(handle_info_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
