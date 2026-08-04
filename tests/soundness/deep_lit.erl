-module(deep_lit).

-compile(export_all).

main() ->
  Y = id([[a], [b, c], {[d], [e]}]),
  Y.

id(X) ->
  X.
