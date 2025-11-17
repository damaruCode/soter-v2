%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     Very simple test program.
%%     This cannot be proven error-free with the current match function.
%%

-module(match).
-compile(export_all).

main() ->
    P = spawn(fun()-> f() end),
    P ! {tellme,self()}, P ! {greet,hi},
    receive
        X -> X
    end,
    "bye".

f() ->
    receive
        {greet, hi}    -> g(english);
        {greet, mandi} -> g(furlan);
        {greet, _}     -> erlang:error('Unknown_language')
    end.

g(R) ->
    receive
        {tellme, P} -> P ! R
    end.

