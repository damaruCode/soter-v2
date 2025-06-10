-module(example).

-compile(export_all).

main() ->
  P = spawn(fun() -> receive {a, X} -> X ! ok end end),
  P ! {a, self()}.
