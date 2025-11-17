-module(bigring).

-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

-soter_config(peano).

-uncoverable("token >= 2").

main() ->
  self() ! 0,
  leader(self()).

leader(Head) ->
  receive
    % N when N > 1000 -> io:fwrite("dead ~w~n", [N]);
    Msg ->
      io:fwrite("~w~n", [Msg]),
      New = spawn(fun() -> slave(Head) end),
      New ! Msg,
      leader(New)
  end.

slave(Next) ->
  ?label_mail(token),
  receive
    Msg ->
      Next ! Msg + 1,
      slave(Next)
  end.
