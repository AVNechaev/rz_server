%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Март 2016 18:37
%%%-------------------------------------------------------------------
-module(web_patterns).
-author("user").

%% API
-export([
  init/1,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  patterns_list/2,
  process_post/2]).

-include("internal.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['GET', 'POST'], ReqData, Context}.
content_types_provided(ReqData, Context) -> {[{"application/json", patterns_list}], ReqData, Context}.
content_types_accepted(ReqData, Context) -> {[{"application/json", process_post}], ReqData, Context}.

%%--------------------------------------------------------------------
patterns_list(ReqData, Context) ->
  {ok, Data} = pattern_store:get_patterns_indexes(),
  Payload = mochijson2:encode([{struct, [{id, Id}]} || Id <- Data]),
  {Payload, ReqData, Context}.

%%--------------------------------------------------------------------
process_post(ReqData, Context) ->
  try
    {struct, [{<<"text">>, PatText}]} = mochijson2:decode(wrq:req_body(ReqData)),
    case pattern_store:add_pattern(PatText) of
      {ok, #pattern{idx = Id}} ->
        lager:info("Created pattern ~p as ~p", [PatText, Id]),
        {{halt, 201}, wrq:set_resp_body(["{""id"":",integer_to_binary(Id), "}"], ReqData), Context};
      {error, Reason} ->
        lager:warning("HTTP handler warning:~p", [Reason]),
        {{halt, 400}, wrq:set_resp_body(<<"{""error"":""couldn't compile pattern""}">>, ReqData), Context}
    end
  catch
    M:E ->
      lager:warning("HTTP handler error: ~p:~p; ~p", [M, E, erlang:get_stacktrace()]),
      {{halt, 400}, wrq:set_resp_body(<<"{""error"":""couldn't parse request body""}">>, ReqData), Context}
  end.
