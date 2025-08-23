-module(rec_id).

-compile(export_all).

main() ->
  Y = id({a}),
  Z = id(b),
  {Y, Z}.

id(X) ->
  id2(X).

id2(X) ->
  X.
