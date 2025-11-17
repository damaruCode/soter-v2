%%  Author: Emanuele
%%  Created: 05/06/2012
%%  Description:
%%
%%     Test for error discovery.
%%

-module(safe_send).

-compile(export_all).

-include_lib("soter.hrl").

main() ->
    ME = self(),
    S = spawn(fun()-> receive {_,X} -> ME ! ok, server(X) end end),
    S ! {hi, init},
    receive ok -> ok end,
    S ! {hi, self()},
    S ! {then, self()},
    S ! bye.

server(State) ->
    receive
        {X, P} -> P ! X, % This call would throw an exception if matching P=init.
                  server(X);
        bye    -> ?label(stop_server), ok
    end.

