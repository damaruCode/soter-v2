%%  Author: Emanuele
%%  Created: 05/06/2012
%%  Description:
%%
%%     Test for error discovery.
%%     This example requires reasoning about VAS counters in order
%%     to be proved correct and the CFA alone does not suffice to prove the property.
%%

-module(parikh).

-compile(export_all).

-include_lib("soter.hrl").

main() ->
    S = spawn(fun()-> receive {init, P, I} -> P ! ok, server(I), P ! 'DOWN' end end),
    S ! {init, self(), bla},
    receive ok -> ok end,
    S ! {hi, self()},
    S ! {then, self()},
    S ! bye,
    receive 'DOWN' -> ok end,
    ?label(monitor_ok).

server(State) ->
    receive
        {init, _, _} ->
            ?soter_error("We should be already initialized!");
        {X, P} ->
            P ! State,
            server(X);
        bye ->
            ?label(stop_server),
            ok
    end.

