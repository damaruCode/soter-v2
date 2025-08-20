-module(rec_id_ext).

-compile(export_all).

main() ->
  Y = id(a),
  Z = id(b),
  Y = Z.

id(X) ->
  X.

id2(X) ->
  id(X).
