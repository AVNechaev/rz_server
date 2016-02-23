-ifndef(INTERNAL_HRL).
-define(INTERNAL_HRL, true).

-include_lib("iqfeed_client/include/iqfeed_client.hrl").

-record(candle, {
  name :: instr_name(),
  open = 0 :: float(),
  close = 0 :: float(),
  high = 0 :: float(),
  low = 0 :: float(),
  vol = 0 :: float(),
  bid = 0 :: float(), % bid & ask используются только в текущей свече
  ask = 0 :: float()
}).

-type pattern_index() :: non_neg_integer().
-type pattern_text() :: binary() | string().

-record(pattern, {
  idx :: pattern_index(),
  text :: pattern_text(),
  md5 :: binary()
}).

-type pattern_fun() :: fun((Instr :: instr_name()) -> ok).
-endif.