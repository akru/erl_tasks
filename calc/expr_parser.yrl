Terminals '~' '(' ')' '+' '-' '*' '/' num.
Nonterminals E.
Rootsymbol E.

Left 100 '-'.
Left 100 '+'.
Left 200 '*'.
Left 200 '/'.
Unary 300 '~'.

E -> '(' E ')'  : '$2'.
E -> E '+' E    : {token('$2'), '$1', '$3'}.
E -> E '-' E    : {token('$2'), '$1', '$3'}.
E -> E '*' E    : {token('$2'), '$1', '$3'}.
E -> E '/' E    : {token('$2'), '$1', '$3'}.
E -> '~' E      : {token('$1'), '$2'}.
E -> num        : value('$1').

Erlang code.

value({_, _, Value}) -> Value.
token({Token, _}) -> Token.
