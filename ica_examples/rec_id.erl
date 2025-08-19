-module(rec_id).

-compile(export_all).

main() ->
  Y = id(a),
  Z = id(b).

id(X) ->
  X.

id2(X) ->
  id(X).
