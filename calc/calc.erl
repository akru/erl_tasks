-module(calc).
-export([
    parse/1
  , eval/1
  , show/1
]).

parse(Expr) ->
    {ok, Tokens, _} = expr_lexer:string(Expr),
    expr_parser:parse(Tokens).

eval(Expr) ->
    {ok, E} = parse(Expr),
    eval_(E).

eval_({'~', A}) -> - eval_(A);
eval_({'+', A, B}) -> eval_(A) + eval_(B);
eval_({'-', A, B}) -> eval_(A) - eval_(B);
eval_({'*', A, B}) -> eval_(A) * eval_(B);
eval_({'/', A, B}) -> eval_(A) / eval_(B);
eval_(A) -> A.

show({'~', E}) -> "~ " ++ show(E);
show({'+', A, B}) -> "(" ++ show(A) ++ ") + (" ++ show(B) ++ ")";
show({'-', A, B}) -> "(" ++ show(A) ++ ") - (" ++ show(B) ++ ")";
show({'*', A, B}) -> "(" ++ show(A) ++ ") * (" ++ show(B) ++ ")";
show({'/', A, B}) -> "(" ++ show(A) ++ ") / (" ++ show(B) ++ ")";
show(E) -> integer_to_list(E).
