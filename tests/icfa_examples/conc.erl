-module(conc).

-compile(exports_all).

main() ->
  DB = spawn(fun() -> dataBase([]) end),
  DB.

dataBase(L) ->
  L.
