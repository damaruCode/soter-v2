-module(case_values).

-compile(exports_all).

main() ->
  A = id(2),
  B = id(a),
  C = case [A, B] of
    [M, 1] -> M;
    [3, M] -> M;
    X -> 14
  end,
  C.

id(X) ->
  X.
