%%%-------------------------------------------------------------------
%%% @author anechaev
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 23:27
%%%-------------------------------------------------------------------
-module(sma_deepupdate).
-author("anechaev").

%% API
-export([]).

exec_instr(Instr, Table) ->

%%   TODO: mysqldump -> csv, process, load into, rename; os:cmd(Cmd),
  Q20 = sma_queue:new(20),
  Q50 = sma_queue:new(50),
  Q200 = sma_queue:new(200).