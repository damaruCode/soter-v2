-module(case_values).

-compile(exports_all).

main() ->
  A = id(2),
  B = id(a),
  T = {A, B},
  C = case T of
    {M, 1} -> M;
    {3, M} -> M;
    X -> 14
  end,
  C.

id(X) ->
  X.
