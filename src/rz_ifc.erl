%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Февр. 2016 19:41
%%%-------------------------------------------------------------------
-module(rz_ifc).
-author("user").

%% API
-export([load_instrs_from_file/1, load_instrs/1]).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

%%--------------------------------------------------------------------
-spec load_instrs(Instrs :: [instr_name()]) -> ok.
load_instrs(Instrs) ->
  [
    online_history_worker:set_instrs(online_history_worker:reg_name(N), Instrs)
    || {N, _} <- rz_util:get_env(rz_server, frames)
  ],
  iql1_conn:set_instrs(Instrs),
  ok.

%%--------------------------------------------------------------------
-spec load_instrs_from_file(Filename :: file:filename()) -> ok.
load_instrs_from_file(Filename) ->
  {ok, I} = rz_util:load_instr_csv(Filename, 1, rz_util:get_env(iqfeed_client, instr_defaults)),
  load_instrs(I).
