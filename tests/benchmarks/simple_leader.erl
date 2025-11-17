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

main() -> testtnode().
%%% Three possible entry points
testtnode() -> start(fun(Out,Notary,X)->tnode(Out,Notary,X) end).
testsnode() -> start(fun(Out,Notary,X)->snode(Out,Notary,X) end).
testdnode() -> start(fun(Out,Notary,X)->dnode(Out,Notary,X) end).


start(F) ->
    init_ring(F, [b,a,c], self()),
    receive
        elected -> congratulations
                      %, io:fwrite("Hail to ~w, the new leader!~n",[Leader])
    end.

init_ring(Fun,[Hd|Rst],Notary) ->
    Pnew = spawn(fun()->
                    receive {out,Out} -> Fun(Out,Notary,Hd) end
                 end),
    ring(Fun,Rst,Notary,Pnew,Pnew).

ring(_, [], _, Pstop, Pprev) ->
    Pstop!{out,Pprev}, Pstop;
ring(Fun,[Hd|Rst],Notary,Pstop,Pprev) ->
    Pnew = spawn(fun()->Fun(Pprev,Notary,Hd) end),
    ring(Fun,Rst,Notary,Pstop,Pnew).

%FIRST ALG

tnode(Out,Notary,D) -> Out!D, tnodeB(Out,Notary,D).

tnodeB(Out,Notary,D) ->
    receive E ->
        case compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            gt -> tnode(Out,Notary,E);
            lt -> tnodeB(Out,Notary,D)
        end
    end.

% SECOND ALG

snode(Out,Notary,D) ->
    Out!D,
    receive E ->
        case compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            gt -> snode(Out,Notary,E);
            lt -> c(Out)
        end
    end.

%%%% Dolev, Klawe and Rodeh ALG

c(Out) -> receive V -> Out!V, c(Out) end.

dnode(Out,Notary,D) ->
    Out!D,
    receive E ->
        case compare(E,D) of
            eq -> ?label(the_leader), Notary!elected;
            _ ->
                Out!E,
                receive F ->
                    case {compare(E,D),compare(E,F)} of
                        {gt, gt} -> dnode(Out,Notary,E);
                        _        -> c(Out)
                    end
                end
        end
    end.

rule(Out,D)-> leader.


compare(a,a)->eq;
compare(a,b)->lt;
compare(a,c)->lt;
compare(b,a)->gt;
compare(b,b)->eq;
compare(b,c)->lt;
compare(c,a)->gt;
compare(c,b)->gt;
compare(c,c)->eq.


