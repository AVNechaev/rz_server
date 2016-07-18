%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Март 2016 18:37
%%%-------------------------------------------------------------------
-module(web_instruments).
-author("user").

%% API
-export([
  init/1,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  process_put/2]).

-include_lib("webmachine/include/webmachine.hrl").

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['PUT'], ReqData, Context}.
content_types_provided(ReqData, Context) -> {[{"application/json", instr_list}], ReqData, Context}.
content_types_accepted(ReqData, Context) -> {[{"application/json", process_put}], ReqData, Context}.

%%--------------------------------------------------------------------
process_put(ReqData, Context) ->
  try
    Raw = mochijson2:decode(wrq:req_body(ReqData)),
    Instrs = [V || {struct, [{<<"name">>, V}]} <- Raw],
    {Added, Duplicates} = rz_ifc:load_instrs(Instrs),
    Resp = [
      "{""added"":",
      integer_to_binary(Added),
      """duplicates"":",
      integer_to_binary(Duplicates),
      "}"
    ],
    {{halt, 200}, wrq:set_resp_body(Resp, ReqData), Context}
  catch
    M:E ->
      lager:warning("HTTP handler error: ~p:~p; ~p", [M, E, erlang:get_stacktrace()]),
      {{halt, 400}, ReqData, Context}
  end.