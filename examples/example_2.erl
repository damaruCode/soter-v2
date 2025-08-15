-module(example_2).

-compile(export_all).

main() ->
  C = a,
  case {a, b, c} of
    {b, c, a} ->
      R = a
  end.
