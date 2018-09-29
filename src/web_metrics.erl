%%%-------------------------------------------------------------------
%%% @author an
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2018 12:24
%%%-------------------------------------------------------------------
-module(web_metrics).
-author("an").

%% API
-export([init/1, allowed_methods/2, content_types_provided/2, get/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ===================================================================
init([]) -> {ok, undefined}.
allowed_methods(Req, Ctx) -> {['GET'], Req, Ctx}.
content_types_provided(ReqData, Context) -> {[{"text/xml", get}], ReqData, Context}.

%%--------------------------------------------------------------------
get(Req, Ctx) -> {prometheus_text_format:format(), Req, Ctx}.