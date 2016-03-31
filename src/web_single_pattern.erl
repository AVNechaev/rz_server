%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2016 22:17
%%%-------------------------------------------------------------------
-module(web_single_pattern).
-author("anechaev").

%% API
-export([
  init/1,
  delete_resource/2,
  allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['DELETE'], ReqData, Context}.

%%--------------------------------------------------------------------
delete_resource(ReqData, Context) ->
  try
    Id = list_to_integer(wrq:path_info(pattern_id, ReqData)),
    case pattern_store:remove_pattern(Id) of
      ok ->
        lager:info("Deleted pattern #~p", [Id]),
        {{halt, 204}, ReqData, Context};
      {error, not_found} ->
        lager:info("Couldn't delete pattern #~p: not found", [Id]),
        {{halt, 404}, ReqData, Context}
    end
  catch
    M:E ->
      lager:warning("HTTP handler error: ~p:~p; ~p", [M, E, erlang:get_stacktrace()]),
      {{halt, 400}, ReqData, Context}
  end.