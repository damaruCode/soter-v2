-module(scalaris).
-compile(export_all).

-include_lib("soter.hrl").

main() ->
    S = start(),
    mgclient(S).

mgclient(S) ->
  ?label('client'),
  S! ?SoterOneOf([{rd , self()},{qry, self()},{mod, fun(X)-> {smth, X} end}]),
  mgclient(S).

start() ->
  spawn( fun() ->
    AL = self(),
    SL = spawn(fun()->synchloop(AL, empty) end),
    asyncloop(SL, empty)
  end).

asyncloop(SL, State) ->
  receive
    {upd, New} -> % this is not meant to be used by client
      asyncloop(SL, New);
    {rd,  P  } ->
      P ! State,
      asyncloop(SL, State);
    {qry, P  } ->
      spawn(fun() -> answer(State, P) end),
      asyncloop(SL, State);
    {mod, F}   ->
      SL ! {mod, F},
      asyncloop(SL, State)
  end.

answer(State, P) ->
  % expensive operation on State,
    P ! ok.

synchloop(AL, State) ->
  ?label_mail("sl"),
  receive
    {mod, F} ->
      New = F(State),
      AL ! {upd, New},
      synchloop(AL, New);
    _ -> ?soter_error("Unexpected message to synchloop")
  end.
