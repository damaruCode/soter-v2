-module(concurr).

-compile(export_all).

main() ->
  P = spawn(fun() -> receive {A, M} -> A ! M end end),
  P ! {self(), a},
  receive
    M ->
      P ! M
  end.
