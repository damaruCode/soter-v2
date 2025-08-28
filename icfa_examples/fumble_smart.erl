-module(fumble_smart).

-compile(exports_all).

main() ->
  Y = id(a),
  Z = id(b),
  {Y, Z}.

id(X) ->
  A = a,
  id2(X).

id2(X) ->
  X.
