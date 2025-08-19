-module(rec_id).

-compile(export_all).

main() ->
  Y = id(a),
  Z = id(b).

id(X) ->
  id(X).
