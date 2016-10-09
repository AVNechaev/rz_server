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
-export([exec/1]).
-include("internal.hrl").

exec(Table) ->
  Q20 = sma_queue:new(20),
  Q50 = sma_queue:new(50),
  Q200 = sma_queue:new(200),

  InFName = "in_" ++ Table,
  CmdSel = "mysql -urz_writer -p123456 CANDLES -e \"SELECT name, ts, open, high, low, close, volume FROM " ++ Table ++  " ORDER BY ts\" >" ++ InFName,
  os:cmd(CmdSel),
  {ok, InH} = file:open(InFName, [read, raw, binary]),
  {ok, OutH} = file:open("out_" ++ Table, [write, raw, binary]),
  ok = rl(file:read_line(InH), InH, OutH, Q20, Q50, Q200),
  ok = file:close(OutH),
  ok = file:close(InH).

rl(eof, _In, _Out, _S20, _S50, _sS00) -> ok;
rl({ok, Data}, InH, OutH, S20, S50, S200) ->
  [Name, TS, O, H, L, C, V] = binary:split(Data, <<"\t">>, [global]),
  Price = binary_to_float(C),
  N20 = sma_queue:store(Price, S20),
  N50 = sma_queue:store(Price, S50),
  N200 = sma_queue:store(Price, S200),
  R = [
    Name, ",",
    TS, ",",
    O, ",",
    H, ",",
    L, ",",
    C, ",",
    float_to_binary(N20#sma_q.val), ",",
    float_to_binary(N50#sma_q.val), ",",
    float_to_binary(N200#sma_q.val), ",",
    V
  ],
  ok = file:write(OutH, R),
  rl(file:read_line(InH), InH, OutH, N20, N50, N200).