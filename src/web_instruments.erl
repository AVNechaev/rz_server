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
  content_types_provided/2
  , content_types_accepted/2]).

-include_lib("webmachine/include/webmachine.hrl").

-compile([{parse_transform, lager_transform}]).
%% ===================================================================

init([]) -> {ok, undefined}.
allowed_methods(ReqData, Context) -> {['GET', 'PUT', 'POST'], ReqData, Context}.
content_types_provided(ReqData, Context) -> {[{"application/json", instr_list}], ReqData, Context}.
content_types_accepted(ReqData, Context) -> {[{"application/json", process_post}], ReqData, Context}.

%%--------------------------------------------------------------------
