-module(report).

-compile(export_all).

main() ->
  P = spawn(fun() -> receive X -> X ! ok end end),
  P ! self(),
  receive
    ok ->
      P ! self()
  end.
