-module(report).

-compile(export_all).

main() ->
  P = spawn(fun() -> receive {a, X} -> X ! ok end end),
  P ! {a, self()},
  receive
    ok ->
      self();
    _ ->
      "Yay"
  end.
