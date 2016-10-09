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
  InFName = "in_" ++ Table,
  CmdSel = "mysql -urz_writer -p123456 CANDLES -e \"SELECT name, ts, open, high, low, close, volume FROM " ++ Table ++  " ORDER BY ts\" | tail -n +2 >" ++ InFName,
  os:cmd(CmdSel),
  {ok, InH} = file:open(InFName, [read, raw, binary]),
  {ok, OutH} = file:open("out_" ++ Table, [write, raw, binary]),
  T = ets:new(local, [private, set]),
  ok = rl(file:read_line(InH), InH, OutH, T),
  ets:delete(T),
  ok = file:close(OutH),
  ok = file:close(InH).

rl(eof, _In, _Out, _T) -> ok;
rl({ok, Data}, InH, OutH, T) ->
  [Name, TS, O, H, L, C, V] = binary:split(Data, <<"\t">>, [global]),

  Price = binary_to_float(C),
  R = [
    Name, ",",
    TS, ",",
    O, ",",
    H, ",",
    L, ",",
    C, ",",
    float_to_binary(store(T, Name, 20, Price), [{decimals, 4}]), ",",
    float_to_binary(store(T, Name, 50, Price), [{decimals, 4}]), ",",
    float_to_binary(store(T, Name, 200, Price), [{decimals, 4}]), ",",
    V
  ],
  ok = file:write(OutH, R),
  rl(file:read_line(InH), InH, OutH, T).

store(T, Instr, SMADepth, V) ->
  Key = {Instr, SMADepth},
  Q = case ets:lookup(T, Key) of
        [] -> New = sma_queue:new(SMADepth),
          true = ets:insert_new(T, {Key, New}),
          New;
        [{Key, Val}] -> Val
      end,
  NQ = sma_queue:store(V, Q),
  ets:update_element(T, Key, {2, NQ}),
  NQ#sma_q.val.