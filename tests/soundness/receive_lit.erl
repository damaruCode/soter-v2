-module(receive_lit).

-compile(exports_all).

main() ->
  P = spawn(fun() -> receive {X, M} -> X ! M end end),
  P ! package(a),
  receive 
    X -> X
  end.

package(M) -> 
  {self(), M}.
