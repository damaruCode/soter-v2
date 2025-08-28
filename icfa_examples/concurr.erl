-module(concurr).

-compile(export_all).

main() ->
  F = fun() -> receive {A, M} -> A ! M end end,
  P = spawn(F),
  P ! {self(), a},
  receive
    M ->
      P ! M
  end.
