-module(rec_id).

-compile(export_all).

main() ->
  X = 1,
  Y = id(id(X)).

id(X) ->
  X.
