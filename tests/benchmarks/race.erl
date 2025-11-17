%%  Author: Emanuele
%%  Created: 4/6/2012
%%  Description:
%%
%%     A process implementing a counter with locks shared between two
%%     identical clients trying to increment it concurrently.
%%     A mutual exclusion property of the section of the clients
%%     protected by locks is established.


-module(race).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

%%% CELL
free_cell(X) ->
    receive
        {lock, P} ->
            P ! {ready, self()},
            locked_cell(P, X)
    end.

locked_cell(P,X) ->
    receive
        {write, P, Y} ->
            locked_cell(P,Y);
        {read, P} ->
            P ! {val, self(), X},
            locked_cell(P,X);
        {unlock, P} ->
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


%%% INCREMENT CLIENT

inc(C) ->
    lock_cell(C),
    ?assert_uncoverable(2),
    ?label(critical),
    write_cell(C, {s,read_cell(C)}),
    unlock_cell(C).

%%% Entry point

main() ->
    C = spawn(fun() -> free_cell(?any_peano()) end),
    spawn(fun()-> inc(C) end),
    spawn(fun()-> inc(C) end).
