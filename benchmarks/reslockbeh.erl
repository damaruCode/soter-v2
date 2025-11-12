%%  Author: Emanuele
%%  Created: 1/5/2012
%%  Description:
%%
%%    The code has three logical parts, which would constitute three modules in
%%    Erlang. The first part defines an Erlang _behaviour_ that governs the
%%    lock-controlled, concurrent access of a shared resource by a number of
%%    clients. A resource is viewed as a function implementing a protocol that
%%    reacts to requests; the function is called only when the lock is acquired.
%%    Note the use of higher-order arguments and return values.
%%
%%    The second part implements a mock implementation of the `Res` callback:
%%    `inst` defines a resource that will react arbitrarily to arbitrary
%%    parameters.
%%
%%    The last part defines `any_client`, a simple function that simulates a
%%    generic client for the shared resource. It acquires a lock, performs an
%%    arbitrary all number of arbitrary actions on the resource, unlocks it and
%%    then starts over again. The function `many_clients` spawns N instances of
%%    `any_client` all trying to use the same resource.
%%
%%    This example aims to demonstrate the ability of Soter in handling:
%%
%%    - Behaviour-like modules (via higher-order)
%%    - Non determinism (the `soter:choice` operator)
%%    - Message-passing
%%    - Dynamic Creation of processes
%%    - Rich concurrency-sensitive properties as mutual exclusion
%%


-module(reslockbeh).
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


%%% Generic Instance

inst_start() -> res_start(inst(?any_peano())).
inst(_X) ->
    fun(_P, _Cmd)->
        {
            inst(?any_peano()),
            ?SoterOneOf([
                ok,
                { reply, any_ans() }
            ])
        }
    end.

any_ans() ->
    ?SoterOneOf([
        ans1,
        {ans2,?any_peano()}
    ]).


%%% GENERIC CLIENT

any_client(C) ->
    res_lock(C),
    ?label(critical),
    any_interaction(C),
    res_unlock(C),
    any_client(C).

any_interaction(C) ->
    soter:choice(
        fun() -> ok end,
        fun() -> res_do(C, ?SoterOneOf([cmd1,{cmd2, ?any_peano()}])), any_interaction(C) end,
        fun() -> res_request(C, ?SoterOneOf([req1,{req2, ?any_peano()}])), any_interaction(C) end
    ).

%%% Program Entry points

main() ->
    C = inst_start(),
    many_clients(C, ?any_peano()).

% ?any_peano is defined in grammars.hrl (automatically included)
% and generates all the terms of the form X ::= zero | {s, X}

many_clients(_, zero) -> ok;
many_clients(C, {s, N}) ->
    spawn(fun()->any_client(C) end),
    many_clients(C, N).


