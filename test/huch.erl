%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     Shared database adapted from Huch's paper.
%%     the read.. functions were originally performing IO
%%     while here they are abstracted using soter:choice and
%%     returning all possible relevant values.
%%


-module(huch).
-compile(export_all).

-include_lib("soter.hrl").

%%% See huch.prop for the property to prove.
%%% Equivalent to:
%%% -uncoverable("client_writes >= 2").

main() ->
    DB = spawn(fun()->dataBase([])end),
    spawn(fun()->client(DB) end),
    spawn(fun()->client(DB) end).


dataBase(L) ->
    receive
        {allocate,Key,P} ->
            case lookup(Key,L) of
                fail ->
                    P!free,
                    receive
                        {value,V,P} ->
                            dataBase([{Key,V}|L])
                    end;
                {succ,V} ->
                    P!allocated,
                    dataBase(L)
            end;
        {lookup,Key,P} ->
            P!lookup(Key,L),
            dataBase(L)
    end.

lookup(K,L) ->
    case L of
        []        -> fail;
        [{K,V}|_] -> {succ,V};
        [_|Xs]    -> lookup(K,Xs)
    end.

client(DB) ->
    case read() of
        {ok,i} ->
            K = readKey(),
            DB!{allocate,K,self()},
            receive
                free ->
                    V = readVal(),
                    ?label(client_writes),
                    DB!{value,V,self()},
                    client(DB);
                allocated ->
                    ?label(client_denied),
                    client(DB)
            end;
        {ok,l} ->
            K = readKey(),
            DB!{lookup,K,self()},
            receive
                fail -> ?label(client_fail),client_not_found(DB, K);
                {succ,V} -> ?label(client_reads), client_found(DB, K, V)
            end,
            client(DB)
        %{ok,q} -> io:fwrite("~s quit~n",[]);
        %X -> client(DB)
    end.

client_found(DB,_,_) -> client(DB).
client_not_found(DB,_) -> client(DB).

read() -> ?SoterOneOf([{ok,i}, {ok,l}]).
%read() -> {ok,q}.

readVal() -> ?SoterOneOf([a, b, c]).

readKey() -> ?SoterOneOf([k1, k2, k3]).


