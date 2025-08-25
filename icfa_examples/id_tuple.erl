-module(id_tuple).

-compile(export_all).

main() ->
  Y = id({a, b}),
  Z = id({c, d}),
  {Y, Z}.

id(X) ->
  X.
