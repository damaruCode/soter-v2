-module(church).

-compile(exports_all).

main() ->
  ping(self()).

ping(P) ->
  pong(P).

pong(P) ->
  ping(spawn(fun() -> ping(self()) end)).
