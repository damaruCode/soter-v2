-module(cons_test).

-compile(export_all).

main() ->
  Y = [id(a), id(b)],
  Y.

id(X) ->
  X.
