-module(recurr).

-compile(exports_all).

main() ->
  X = recurr(a),
  Y = recurr(X),
  {X, Y}.

recurr(A) ->
  case A of
    a ->
      recurr(b);
    b ->
      recurr(c);
    c ->
      recurr(d);
    d ->
      e;
    e ->
      a
  end.
