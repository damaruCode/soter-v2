-module(thesis).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

% -soter_config(peano).
-uncoverable("critical >= 2").
-uncoverable("client >= 2").

% main() -> main(?any_nat()).

main() ->
  DB = spawn(fun() -> database(?any_peano()) end),
  Server = spawn(fun()-> serve(DB) end),
  % spawn(fun()-> pinger(Server) end),
  spawn_clients(Server),
  % Server ! {ping, self()},
  DB.
% main() ->
%   DB = spawn(fun()-> database({s,0}) end),
%   Server = spawn(fun()-> serve(DB) end),
%   pinger(Server),
%   spawn_clients(Server),
%   io:fwrite("OK~n"),
%   DB.

spawn_clients(S) ->
  io:fwrite("Client spawned~n"),
  spawn(fun()-> client(S) end),
  % spawn(fun()-> pinger(S) end),
  % spawn(fun()-> S ! {ping, self()} end),
  case ?any_bool() of
    true  -> spawn_clients(S);
    _     -> ?label(bla),ok
  end.
  % spawn(fun()-> client(S) end),
  % spawn(fun()-> S ! {ping, self()} end),
  % spawn_clients(S).

pinger(Server) ->
  Server ! {ping, self()},
  pinger(Server).

client(Server) ->
  ?label_mail("client"),
  Server ! {incr, self()},
  receive
    {ans, C} -> io:fwrite("Client received ~w~n", [C])
                , client(Server)
  end
  .
  % , ok.

serve(DB) ->
  % io:fwrite("Server up!~n"),
  receive
    {ping , P} ->
      P ! {pong, self()},
      io:fwrite("Server pinged~n");
    {count, C} -> spawn(fun()-> worker(DB, fun(X) -> X end, C) end);
    {incr , C} -> spawn(fun()-> worker(DB, fun(X) -> {s, X} end, C) end)
  end,
  serve(DB).

database(State) ->
  io:fwrite("DB available [~w]~n", [State]),
  receive {lock, C} ->
      C ! {ready, self()},
      io:fwrite(">> DB LOCKED [~w]!~n", [C]),
      operate_db(State, C)
  end.

operate_db(State, Owner) ->
  io:fwrite("   DB operating [~w]~n", [State]),
  receive
    {read, Owner} ->
      Owner ! {val, self(), State},
      operate_db(State, Owner);
    {write, X, Owner} ->
      % Owner ! {val, self(), State},
      operate_db(X, Owner);
    {unlock, Owner} ->
      io:fwrite("<< DB UNLOCKED [~w]!~n", [Owner]),
      database(State)
  end.

worker(DB, F, C) ->
  DB ! {lock, self()},
  receive {ready, DB} ->
    ?label(critical),
    DB ! {read, self()},
    receive {val, DB, X} ->
      DB ! {write, F(X), self()},
      % receive {val, DB, _} ->
        DB ! {unlock, self()},
        C  ! {ans, X}
      % end
    end
  end.


