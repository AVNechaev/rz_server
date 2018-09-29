%%%-------------------------------------------------------------------
%%% @author an
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2018 11:38
%%%-------------------------------------------------------------------
-module(metrics).
-author("an").

%% API
-export([init/0, add_tick/1, add_fire/0]).

-include("metrics.hrl").
-type label_val() :: erlang:iodata().

%%%===================================================================
%%% API
%%%===================================================================
init() ->
  ?METRIC_CREATE_COUNTER(ticks, "Incoming ticks by source", [{labels, [source]}]),
  ?METRIC_CREATE_COUNTER(fires, "Patterns fired").

%%--------------------------------------------------------------------
-spec add_tick(Source :: label_val()) -> ok.
add_tick(Source) -> ?METRIC_ADD_COUNTER(ticks, 1, [Source]).

%%--------------------------------------------------------------------
-spec add_fire() -> ok.
add_fire() -> ?METRIC_ADD_COUNTER(fires, 1).
