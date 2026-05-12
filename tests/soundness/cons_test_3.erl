-module(cons_test).

-compile(export_all).

main() ->
  Y = ['a', 'b'],
  Z = "ab",
  test(Y),
  test(Z),
  {Y, Z}.

test(U) ->
  case U of
    [a, X] -> X;
    [Y, c] -> Y
  end.
