-module(thesis).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

% -soter_config(peano).
-uncoverable("critical >= 2").
-uncoverable("client >= 2").

% main() -> main(?any_nat()).

main() ->
  DB = spawn(fun() -> free_cell(?any_peano()) end),
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
    false -> ok;
    true  -> spawn_clients(S)
  end.
  % spawn(fun()-> client(S) end),
  % spawn(fun()-> S ! {ping, self()} end),
  % spawn_clients(S).

pinger(Server) ->
  Server ! {ping, self()},
  pinger(Server).

client(Server) ->
  ?label_mail(client),
  case ?any_bool() of
    false -> Server ! {ping, self()};
    true  -> Server ! {incr, self()}
  end,
  % receive
  %   _ -> client(Server)
  % end.
  ok.

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

% database(State) ->
%   io:fwrite("DB available [~w]~n", [State]),
%   receive {lock, C} ->
%       C ! {locked, self()},
%       io:fwrite(">> DB LOCKED [~w]!~n", [C]),
%       operate_db(State, C)
%   end.

% operate_db(State, Owner) ->
%   io:fwrite("   DB operating [~w]~n", [State]),
%   receive
%     {read, Owner} ->
%       Owner ! {val, State},
%       operate_db(State, Owner);
%     {write, X, Owner} ->
%       Owner ! ok,
%       operate_db(X, Owner);
%     {unlock, Owner} ->
%       Owner ! {unlocked, self()},
%       io:fwrite("<< DB UNLOCKED [~w]!~n", [Owner]),
%       database(State)
%   end.

worker(DB, F, C) ->
  lock_cell(DB),
  ?label(critical),
  X = read_cell(DB),
  write_cell(DB, F(X)),
  C ! {ans, X},
  unlock_cell(DB).

% worker(DB, F, C) ->
%   DB ! {lock, self()},
%   receive {locked, _} -> ok end,
%   ?label(critical),
%   DB ! {read, self()},
%   receive {val, X} ->
%     DB ! {write, F(X), self()},
%     receive ok -> ok end,
%     C ! {ans, X},
%     DB ! {unlock, self()}
%   end.


%%% CELL
free_cell(X) ->
  io:fwrite("DB available [~w]~n", [X]),
    receive
        {lock, P} ->
      io:fwrite(">> DB LOCKED [~w]!~n", [P]),
            P ! {ready, self()},
            locked_cell(P, X);
        {read, P} ->
            P ! {val, self(), X},
            free_cell(X)
    end.

locked_cell(P,X) ->
    io:fwrite("   DB operating [~w]~n", [X]),
    receive
        {write, P, Y} ->
            locked_cell(P,Y);
        {read, P} ->
            P ! {val, self(), X},
            locked_cell(P,X);
        {unlock, P} ->
            io:fwrite("<< DB UNLOCKED [~w]!~n", [P]),
            free_cell(X)
    end.

%%% CELL API

lock_cell(Q) ->
    Q ! {lock, self()},
    receive
        {ready, Q} -> ok
    end.

unlock_cell(Q) ->
    Q ! {unlock, self()}.

read_cell(Q) ->
    Q ! {read, self()},
    receive
        {val,Q,X} -> X
    end.

write_cell(Q, X) ->
    Q ! {write, self(), X}.



% worker(DB, F, C) ->
%   DB ! {lock, self()},
%   receive {locked, DB} ->
%     ?label(critical),
%     DB ! {read, self()},
%     receive {val, X} ->
%       DB ! {write, F(X), self()},
%       receive ok ->
%         C ! {ans, X},
%         DB ! {unlock, self()}
%       end
%     end
%   end.

% -export ([distribute/1, worker/1]).
% distribute(Xs) -> distribute(Xs, 0).
% distribute([], 0) -> 0;
% distribute([], N) ->
%     receive
%         {ans, Y} ->  Y + distribute([], N-1)
%     end;
% distribute([X | Xs], N) ->
%     C = spawn(?MODULE, worker, [random:uniform(5)*200]),
%     C ! {task, self(), X},
%     distribute(Xs, N+1).

% worker(T) ->
%     timer:sleep(T),
%     receive
%         {task, P, X} -> P ! {ans, X*X}
%     end.