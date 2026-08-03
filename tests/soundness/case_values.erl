-module(case_values).

-compile(exports_all).

main() ->
  A = id(b),
  B = id(a),
  C = case [A, B] of
    [M, c] -> M;
    [d, M] -> M;
    X -> e
  end,
  C.

id(X) ->
  X.
