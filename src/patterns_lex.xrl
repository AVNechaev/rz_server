Definitions.

D = [0-9]
Frame = (D|1|10)
Cmp = (>|<|>=|<=)
WS  = [\000-\s]
%offset - от 1 до 22
HistOffset = ([2-9]|1[0-9]|2[0-2])
HistValue = (OPEN|CLOSE|HIGH|LOW|VOLUME)
CurValue = (OPEN|PRICE|HIGH|LOW|BID|ASK|VOLUME)

Rules.

Instr#{Frame},1#{CurValue}                  : {token, {instr, TokenLine, TokenChars}}.
Instr#{Frame},{HistOffset}#{HistValue}      : {token, {instr, TokenLine, TokenChars}}.

and|AND                                     : {token, {two_op_logic, TokenLine, op_and}}.
or|OR                                       : {token, {two_op_logic, TokenLine, op_or}}.

{Cmp}                                       : {token, {comparator, TokenLine, TokenChars}}.

-?{D}+                                      : {token, {constant, TokenLine, list_to_float(TokenChars ++ ".0")}}.
-?{D}+\.{D}+                                : {token, {constant, TokenLine, list_to_float(TokenChars)}}.

\%                                          : {token, {two_op_arith, TokenLine, op_rem}}.
\+                                          : {token, {two_op_arith, TokenLine, op_plus}}.
-                                           : {token, {two_op_arith, TokenLine, op_minus}}.


{WS}+                                       : skip_token.


Erlang code.