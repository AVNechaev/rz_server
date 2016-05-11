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
  allowed_methods/2,
  content_types_accepted/2,
  process_put/2]).

-include_lib("webmachine/include/webmachine.hrl").

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['PUT','DELETE'], ReqData, Context}.
content_types_accepted(ReqData, Context) -> {[{"application/json", process_put}], ReqData, Context}.

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

%%--------------------------------------------------------------------
process_put(ReqData, Context) ->
  try
    Id = list_to_integer(wrq:path_info(pattern_id, ReqData)),
    {struct, [{<<"text">>, PatText}]} = mochijson2:decode(wrq:req_body(ReqData)),
      case pattern_store:replace_pattern(Id, PatText) of
        ok ->
          lager:info("Pattern ID#~p changed to ~p", [Id, PatText]),
          {{halt, 200}, ReqData, Context};
        {error, not_found} ->
          lager:warning("Couldnt replace the pattern #~p: not found", [Id]),
          {{halt, 404}, ReqData, Context};
        {error, {compile_error, Reason}} ->
          lager:warning("HTTP handler warning:~p", [Reason]),
          {{halt, 400}, wrq:set_resp_body(<<"{""error"":""couldn't compile pattern""}">>, ReqData), Context}
      end
  catch
    M:E ->
      lager:warning("HTTP handler error: ~p:~p; ~p", [M, E, erlang:get_stacktrace()]),
      {{halt, 400}, ReqData, Context}
  end.