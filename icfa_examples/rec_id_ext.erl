-module(rec_id_ext).

-compile(export_all).

main() ->
  Y = id(a),
  Z = id(b),
  W = id(c),
  {Y, Z, W}.

id(X) ->
  id2(X).

id2(X) ->
  id3(X).

id3(X) ->
  X.
