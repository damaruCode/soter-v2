%%  Author: Emanuele
%%  Created: 1/5/2012
%%  Description:
%%
%%    The code has three logical parts, which would constitute three modules in
%%    Erlang. The first part defines an Erlang _behaviour_ that governs the
%%    lock-controlled, concurrent access of a shared resource by a number of clients.
%%    A resource is viewed as a function implementing a protocol that reacts to
%%    requests; the function is called only when the lock is acquired. Note the use of
%%    higher-order arguments and return values.
%%
%%
%%    The second part implements a simple 'shared memory cell' resource that holds a
%%    natural number and allows a client to read its value or overwrite it with a new
%%    one Without locks, a shared resource with such a protocol easily leads to race
%%    conditions.
%%
%%    The last part defines `inc`, a simple function that accesses a locked cell
%%    to increment its value. The function `add_to_cell` adds `inc` to the
%%    contents of the cell by spawning `inc` processes incrementing it concurrently.
%%
%%    This example aims to demonstrate the ability of Soter in handling:
%%
%%    - Behaviour-like modules (via higher-order)
%%    - Message-passing
%%    - Dynamic Creation of processes
%%    - Rich concurrency-sensitive properties as mutual exclusion
%%


-module(dynlockb).
-compile(export_all).

-include_lib("soter.hrl").

-soter_config(peano).
-uncoverable("critical >= 2").

-include_lib("grammars.hrl").


%%% LOCKED RESOURCE

res_start(Res) -> spawn(fun()->res_free(Res) end).

res_free(Res) ->
    receive
        {lock, P} ->
            %%io:format("locking: ~p~n",[P]),
            P ! {acquired, self()},
            res_locked(Res, P)
    end.

res_locked(Res, P) ->
    receive
        {req, P, Cmd} ->
            {NewRes, R} = Res(P, Cmd),
            case R of
                ok ->
                    res_locked(NewRes, P);
                {reply, A} ->
                    P ! {ans, self(), A},
                    res_locked(NewRes, P)
            end;
        {unlock, P} ->
            %%io:format("unlocking: ~p~n",[P]),
            res_free(Res)
    end.

%%% RES API

res_lock(Q) ->
    Q ! {lock, self()},
    receive
        {acquired, Q} -> ok
    end.

res_unlock(Q) ->
    Q ! {unlock, self()}, ok.

res_request(Q, Cmd) ->
    Q ! {req, self(), Cmd},
    receive
        {ans, Q, X} -> X
    end.

res_do(Q, Cmd) ->
    Q ! {req, self(), Cmd}, ok.



%%% CELL Implementation

cell_start() -> res_start(cell(0)).
cell(X) ->
    fun(_P, Cmd)->
        case Cmd of
            {write, Y} -> {cell(Y), ok};
            read -> {cell(X), {reply, X}}
        end
    end.

cell_lock(C) -> res_lock(C).
cell_write(C, X) -> res_do(C, {write, X}).
cell_read(C) -> res_request(C, read).
cell_unlock(C) -> res_unlock(C).


%%% INCREMENT CLIENT

inc(C) ->
    cell_lock(C),
    ?label(critical),
    cell_write(C, cell_read(C)+1),
    cell_unlock(C).

%%% Program Entry points

main() -> start(?any_nat()).

start_hack() ->
    C = cell_start(),
    infspawn(fun()->inc(C)end).

infspawn(F)-> spawn(F), infspawn(F).

%%% Alternative parametric start

start(N) ->
    C = cell_start(),
    sp_clones(fun()->inc(C)end, N).

sp_clones(_, 0) -> ok;
sp_clones(F, N) ->
    spawn(F),
    sp_clones(F, N-1).

