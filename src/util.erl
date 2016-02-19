%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Feb 2016 0:31
%%%-------------------------------------------------------------------
-module(util).
-author("anechaev").

%% API
-export([datetime_to_mysql/1]).

%%--------------------------------------------------------------------
-spec datetime_to_mysql(Date :: calendar:datetime()) -> binary().
datetime_to_mysql({{Y, M, D}, {H, Mi, S}}) ->
  iolist_to_binary(io_lib:format("~4.10.0B.~2.10.0B.~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D, H, Mi, S])).