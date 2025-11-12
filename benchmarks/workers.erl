-module(workers).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

-soter_config(peano).
-uncoverable("critical >= 2").
-uncoverable("client >= 2").

main() -> main(?any_nat()).
main(N) ->
  DB = spawn(fun()-> database(0) end),
  S  = spawn(fun()-> serve(DB) end),
  spawn_clients(N, S).

spawn_clients(0, S) -> ok;
spawn_clients(N, S) ->
  spawn(fun()-> client(S) end),
  spawn_clients(N-1, S).

serve(DB) ->
  receive
    {ping , P} ->
      P ! {pong, self()};
    {visit, C} ->
      spawn(fun()-> worker(DB, fun(X)-> X+1 end, C) end)
  end,
  serve(DB).

database(State) ->
  receive {lock, C} ->
    C ! {ready, self()},
    lockeddb(State, C)
  end.

lockeddb(State, Owner) ->
  receive
    {read, Owner} ->
      Owner ! {val, self(), State},
      lockeddb(State, Owner);
    {write, X, Owner} ->
      lockeddb(X, Owner);
    {unlock, Owner} ->
      database(State)
  end.

worker(DB, F, C) ->
  DB ! {lock, self()},
  receive {ready, DB} ->
    ?label("critical"),
    % Start of critical section
    DB ! {read, self()},
    receive {val, DB, X} ->
      DB ! {write, F(X), self()},
      DB ! {unlock, self()},
    % End of critical section
      C  ! {reply, X}
    end
  end.

client(Server) ->
  ?label_mail("client"),
  Server ! {visit, self()},
  receive _ ->
    client(Server)
  end.