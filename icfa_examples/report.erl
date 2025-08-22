-module(report).

-compile(export_all).

main() ->
  P = spawn(fun() -> receive {X, L} -> X ! {ok, L} end end),
  P ! {self(), [1, 2, 3]},
  receive
    {ok, [A, B, C]} ->
      [C, B, A]
  end.
