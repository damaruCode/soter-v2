-module(rec_id).

-compile(export_all).

main() ->
  X = a,
  Y = id(id(X)).

id(X) ->
  id(X).
