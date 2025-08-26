-module(id_tuple).

-compile(export_all).

main() ->
  Y = id({a}),
  Z = id({b}),
  {Y, Z}.

id(X) ->
  X.
