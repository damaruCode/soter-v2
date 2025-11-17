%% Author: Emanuele
%% Created: 06/mar/2012
%% Description:
%%
%%      A Higher Order function that spawns workers and waits for them to terminate
%%      illustrating non-trivial interplay of higher-order and communication in the
%%      definition of a "combinator" extending a protocol implemented by functions
%%      taken as arguments.
%%
%%      To date, Soter cannot prove anything useful here.
%%

-module(howait).
-compile(export_all).

-include_lib("soter.hrl").

-uncoverable("worker_finished >= 1, wait_over >= 1").

-include_lib("grammars.hrl").


serve()->
    receive
        {req, P, X} ->
            P ! {reply, self(), {s,X}},
            serve()
    end.


client(S,N)->
    S ! {req, self(), N},
    receive
        {reply, S, R} -> R
    end.

sp_wait(F, N) -> sp_wait(F, fun()->[] end, N).
sp_wait(_, G, zero) -> G();
sp_wait(F, G, {s,N}) ->
    S=self(),
    Clone = spawn(fun() ->
                    Res = F({s,N}),
                    ?label(worker_finished),
                    S ! {result, self(), Res }
                  end),
    sp_wait(F, fun() -> receive {result, Clone, R} -> [ R | G() ] end end, N).

main()->
    N = ?any_peano(),
    Server = spawn(fun() -> serve() end),
    sp_wait(fun(X)->client(Server, X) end, N),
    ?label(wait_over).

%%% Property:
%%% For each N any state where start returns and
%%% any of the spawned clients did not terminate
%%% is NOT reachable
