-module(concurr).

-compile(exports_all).

main() ->
  P = spawn(fun() -> receive {X, a} -> X ! ok end end),
  P ! {self(), a},
  receive
    ok ->
      b
  end.
