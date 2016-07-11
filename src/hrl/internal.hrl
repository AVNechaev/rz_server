-ifndef(INTERNAL_HRL).
-define(INTERNAL_HRL, true).

-include_lib("rz_util/include/rz_util.hrl").
-include_lib("iqfeed_client/include/iqfeed_client.hrl").

-type pattern_index() :: non_neg_integer().
-type pattern_text() :: binary() | string().

-record(pattern, {
  idx :: pattern_index(),
  text :: pattern_text(),
  md5 :: binary()
}).

-type pattern_fun_arg() :: {tick, instr_name()} | {candle, instr_name()}.
-type pattern_fun() :: fun((Instr :: pattern_fun_arg) -> ok).
-type stock_open_fun() :: fun(() -> non_neg_integer()).

-define(SNP_PREFIX, <<"SNP">>). %% префикс в паттерне для использования SNP500

-endif.