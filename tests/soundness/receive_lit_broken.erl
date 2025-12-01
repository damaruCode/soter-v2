-module(receive_lit_broken).

-compile(exports_all).

main() ->
  P = spawn(fun() -> receive {X, M} -> X ! M end end),
  P ! {self(), a},
  receive 
    X -> R = X
  end,
  R.
