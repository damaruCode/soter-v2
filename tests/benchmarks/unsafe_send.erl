%%  Author: Emanuele
%%  Created: 04/06/2012
%%  Description:
%%
%%     Test for error discovery.
%%     The verification step discovers a genuine counter-example.
%%

-module(unsafe_send).

-compile(export_all).

-include_lib("soter.hrl").

main() ->
    S = spawn(fun()->server()end),
    S ! {hi, bye}.

server() ->
    receive
        {X, P} -> P ! X, % This call will throw an exception when matching P=bye.
                  server();
        bye    -> ?label(stop_server), ok
    end.

