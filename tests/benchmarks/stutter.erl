%%  Author: Emanuele
%%  Created: 5/jun/2012
%%  Description:
%%
%%     Alternate between discarding and hadling messages.
%%     Soter fails to prove absence of errors because it loses precision
%%     wrt order of arrival.
%%

-module (stutter).
-compile(export_all).

-include_lib("soter.hrl").

main() ->
    P = spawn(fun()-> stutter(fun(Msg)-> dosmt(Msg) end) end),
    sendA(P).

wrong_main() ->
    P = spawn(fun()-> stutter(fun(Msg)-> dosmt(Msg) end) end),
    sendB(P).

dosmt(a) -> ?soter_error("We abhorr 'a's.");
dosmt(b) -> 'we love b'.

sendB(P) -> P!b, sendA(P).
sendA(P) -> P!a, sendB(P).

stutter(F) ->
    receive
        _ -> unstutter(F)
    end.

unstutter(F) ->
    receive
        X -> F(X), stutter(F)
    end.
