%%  Author: Emanuele
%%  Created: 2/3/2012
%%  Description:
%%
%%    - Behaviour-like module for locking a generic resource.
%%    - Implementation of "callbacks" for a shared cell storing a counter
%%    - Creation of 2 clients incrementing the shared cell concurrently (see start/0)
%%    - Creation of N clients and collection of results (see start_read/1)

-module(lockb).
-compile(export_all).

-include_lib("soter.hrl").

-soter_config(peano).

-uncoverable("critical >= 2").

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

main() -> start().

start() ->
    C = cell_start(),
    spawn(fun()->inc(C)end),
    spawn(fun()->inc(C)end).

%%% Alternative parametric start

start(N) ->
    C = cell_start(),
    sp_clones(fun()->inc(C)end, N).

sp_clones(_, 0) -> ok;
sp_clones(F, N) ->
    spawn(F),
    sp_clones(F, N-1).

%%% Alternative "supervisor-like" start
%%% See howait.erl

start_read(N) ->
    C = cell_start(),
    sp_collect(fun()->inc(C)end, fun()-> readcell(C) end, N).

readcell(C) ->
    cell_lock(C),
    X=cell_read(C),
    cell_unlock(C),
    X.

sp_collect(_, G, 0) -> G();
sp_collect(F, G, N) ->
    S=self(),
    Clone = spawn(fun()-> F(), S ! self() end),
    sp_collect(F, fun() -> receive Clone -> G() end end, N-1).

