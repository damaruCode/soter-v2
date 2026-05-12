-module(cons_test).

-compile(export_all).

main() ->
  Y = [a | [b | []]],
  Y.
