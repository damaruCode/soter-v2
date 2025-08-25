-module(id_long_literal).

-compile(export_all).

main() ->
  Y = id({a}),
  Z = id({b}),
  {Y, Z}.

id(X) ->
  W = X,
  W.
