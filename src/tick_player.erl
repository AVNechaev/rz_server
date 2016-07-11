%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2016 10:47
%%%-------------------------------------------------------------------
-module(tick_player).
-author("anechaev").

%% API
-export([run/2, run/1, run/0]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

%%--------------------------------------------------------------------
run() -> run("/tmp/ticks.csv").
run(Filename) ->
  {D, _} = erlang:universaltime(),
  run(Filename, D).
run(FileName, Date) ->
  {ok, Ticks} = file:consult(FileName),
  Names = [timeframe_worker:reg_name(N) || {N, _} <- rz_util:get_env(rz_server, frames)],
  TickFun = fun({tick, I, LP, LV, T, B, A}) ->
    Secs = calendar:datetime_to_gregorian_seconds({Date, T}),
    Tick = #tick{name = I, ask = A, bid = B, last_price = LP, last_vol = LV, time = Secs},
    lists:foreach(fun(N) -> timeframe_worker:add_tick(N, Tick) end, Names)
  end,

  lists:foreach(TickFun, Ticks),
  ok.