Nonterminals pattern logic_value arith_value.

Terminals instr two_op_logic comparator constant two_op_arith.

Rootsymbol pattern.

Left 100 two_op_logic.
Left 200 comparator.
Left 300 two_op_arith.

pattern -> logic_value : '$1'.
logic_value -> logic_value two_op_logic logic_value : {'$2', '$1', '$3'}.
logic_value -> arith_value comparator arith_value : {'$2', '$1', '$3'}.

arith_value -> instr two_op_arith instr : {'$2', '$1', '$3'}.
arith_value -> instr two_op_arith constant : {'$2', '$1', '$3'}.
arith_value -> constant two_op_arith instr : {'$2', '$1', '$3'}.
arith_value -> constant : '$1'.
arith_value -> instr : '$1'.