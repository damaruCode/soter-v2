%%
%% Author: Emanuele
%% Created: 02/may/2012
%%
%% Description:
%%
%%     Three implementations of a leader election protocol
%%     in an unidiretional ring of N processes.
%%     N.B.: The property "Only one leader gets elected"
%%           *cannot* be proven by Soter.
%%           The correctness proof relies too heavily on
%%           the choice of unique identifiers for the paricipants.
%%
%%     It differs with dummy_leader.erl in using atoms as ids leading
%%     to a cleaner CFA graph.
%%
%% Reference: Adapted from Fredlund's PhD Thesis
%%
-module(simple_leader).
-compile(export_all).

-include_lib("soter.hrl").

-uncoverable("the_leader > 1").

-define(compare(A,B),
    case A of
        a -> case B of
                a -> eq;
                b -> lt;
                c -> lt
             end;
        b -> case B of
                a -> gt;
                b -> eq;
                c -> lt
             end;
        c -> case B of
                a -> gt;
                b -> gt;
                c -> eq
             end
    end).


main() -> testtnode().
%%% Three possible entry points
testtnode() -> start(fun(Out,Notary,X)->tnode(Out,Notary,X) end).
testsnode() -> start(fun(Out,Notary,X)->snode(Out,Notary,X) end).
testdnode() -> start(fun(Out,Notary,X)->dnode(Out,Notary,X) end).


start(F) ->
    ring_abc(F, self()),
    receive
        elected -> congratulations
                      %, io:fwrite("Hail to ~w, the new leader!~n",[Leader])
    end.

ring_abc(Fun,Notary) ->
    A = spawn(fun()->receive {out,Out} -> Fun(Out,Notary,a) end end),
    B = spawn(fun()->Fun(A,Notary,b) end),
    C = spawn(fun()->Fun(B,Notary,c) end),
    A ! {out, C}.

init_ring(Fun,[Hd|Rst],Notary) ->
    Pnew = spawn(fun()->
                    receive {out,Out} -> Fun(Out,Notary,Hd) end
                 end),
    ring(Fun,Rst,Notary,Pnew,Pnew).

ring(_, [], _, Pstop, Pprev) ->
    Pstop!{out,Pprev}, Pstop;
ring(Fun,[Hd],Notary,Pstop,Pprev) ->
    Pnew = spawn(fun()->Fun(Pprev,Notary,Hd) end),
    ring(Fun,[],Notary,Pstop,Pnew);
ring(Fun,[Hd|Rst],Notary,Pstop,Pprev) ->
    Pnew = spawn(fun()->Fun(Pprev,Notary,Hd) end),
    ring(Fun,Rst,Notary,Pstop,Pnew).

%FIRST ALG

tnode(Out,Notary,D) -> Out!{token,D}, tnodeB(Out,Notary,D).

tnodeB(Out,Notary,D) ->
    receive {token,E} ->
        case ?compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            gt -> tnode(Out,Notary,E);
            lt -> tnodeB(Out,Notary,D)
        end
    end.

% SECOND ALG

snode(Out,Notary,D) ->
    Out!{token, D},
    receive {token, E} ->
        case ?compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            gt -> snode(Out,Notary,E);
            lt -> c(Out)
        end
    end.

%%%% Dolev, Klawe and Rodeh ALG

c(Out) -> receive V -> Out!{token, V}, c(Out) end.

dnode(Out,Notary,D) ->
    Out!{token, D},
    receive {token, E} ->
        case ?compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            _ ->
                Out!{token, E},
                receive {token, F} ->
                    case {?compare(E,D),?compare(E,F)} of
                        {gt, gt} -> dnode(Out,Notary,E);
                        _        -> c(Out)
                    end
                end
        end
    end.

rule(Out,D)-> leader.





