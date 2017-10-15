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

-type frame_name() :: atom().
-type pattern_fun_arg() :: {tick, frame_name(), #candle{}} | {candle, frame_name(), #candle{}}.
-type pattern_fun() :: fun((Instr :: pattern_fun_arg()) -> boolean()).
-type var_data() :: {VarName :: iodata(), Value :: float() | undefined}.
-type var_fun() :: fun((InstrName :: instr_name()) -> [var_data()]).

-define(SNP_PREFIX, <<"SNP">>). %% префикс в паттерне для использования SNP500

-record(sma_q, {
  data :: tuple(),
  head :: integer(),
  tail :: integer(),
  size :: integer(),
  act_size :: integer(),
  val :: float()
}).

-endif.