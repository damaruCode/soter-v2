-module(id).

-compile(export_all).

main() ->
  Y = id([1,2,3,a]),
  Z = id(b),
  {Y, Z}.

id(X) ->
  [1|X].
