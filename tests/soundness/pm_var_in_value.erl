-module(pm_var_in_value).

-compile(exports_all).

main() ->
  P = spawn(fun() -> receive {P, M} -> P ! M end end),
  P ! package(a),
  R = receive 
    a -> b;
    X -> X
  end,
  R.

package(M) ->
  {self(), M}.
