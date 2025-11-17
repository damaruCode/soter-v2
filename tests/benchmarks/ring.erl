%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     This example defines a higher-order
%%     combinator that spawns a number of identical workers,
%%     each applied to a different task in a list.
%%     It then waits for all the workers to return a result
%%     before collecting them in a list which is subsequently returned.
%%     The desired property is that the combinator only returns
%%     when every worker has sent back its result.
%%
%%     Unfortunately to prove this property, stack reasoning is
%%     required, which is beyond the capabilities of an ACS.
%%
%%     This example has been designed to show some limitiations
%%     of Soter with respect to the control-flow abstraction:
%%     no finite state abstraction for the control-states can
%%     capture precisely the behaviour of the stack.
%%

-module(ring).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

-uncoverable("slave_mb >= 2").

main()->
    P = init_ring(fun(A,B)->slave(A,B) end,?list_of_peanos()),
    probe_ring(P).

probe_ring(P) ->
    P ! {peek, zero, self()},
    receive
        {ans, _} -> hurray
    end,
    probe_ring(P).

init_ring(Fun,List) ->
    spawn(fun()-> bootstrap_ring(Fun,List) end).

bootstrap_ring(Fun, Xs) ->
    bootstrap_ring(Fun, self(), Xs).
bootstrap_ring(Fun, Prev, [X]) ->
    Fun(Prev, X);
bootstrap_ring(Fun, Prev, [X | Xs]) ->
    Nxt = spawn(fun()-> Fun(Prev, X) end),
    bootstrap_ring(Fun, Nxt, Xs).


slave(Nxt, Me) ->
    ?label_mail('slave_mb', Nxt),
    receive
        {forward, X} -> Nxt ! {forward, [Me|X]};
        {peek, X, From} ->
            Nxt ! {forward, [Me|X]},
            receive
                {forward, Y} -> From ! {ans, Y}
            end
    end,
    slave(Nxt, Me).
