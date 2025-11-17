%%  Author: Emanuele
%%  Created: 26/jul/2012
%%  Description:
%%
%%     Eratosthenes's Sieve, concurrent flavour.
%%     A counter acts as a generator of integers.
%%     These integers get forwarded or blocked by layers of filter processes
%%     each blocking the multiples of an (already produced) prime number.
%%     When a new prime number is produced a new filter for it is created.
%%     The main just prints all the messages that make it through the layers of filters.
%%
%%     Soter can prove that all the mailboxes have at most 1 message queued.
%%



-module (sieve2).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

-soter_config(peano).

-uncoverable("filter_mail >= 2").
-uncoverable("sieve_mail >= 2").

main() ->
    Me = self(),
    S = spawn(fun()->sieve(Me)end),
    spawn(fun()->counter(S, 2)end),
    dump().

dump() ->
    receive
        X ->
            io:fwrite("~w~n", [X]),
            dump()
    end.


counter(S, N) ->
    ?label_mail("counter_mail"),
    S ! {ans, N},
    counter(S, N+1).

sieve(Out) ->
    ?label_mail("sieve_mail"),
    receive
        {ans, X} ->
            Out ! X,
            S = spawn(fun()->sieve(Out)end),
            filter(X,S)
    end.

filter(X, Out) ->
    ?label_mail("filter_mail"),
    receive
        {ans,Y} ->
            case divisible(Y,X) of
                false -> Out ! {ans, Y}, filter(X, Out);
                true  -> filter(X, Out)
            end
    end.


% Arithmetic operators are not supported by Soter
% A simple workaround is manually providing an abstraction
% for them.

% Here the macro SOTER allows us to use the normal definition
% when running the program through the interpreter and
% to use the mock one instead when analysing it with Soter.

-ifdef(SOTER).

    divisible(Y,X) -> ?any_bool().

-else.

    divisible(Y,X) ->
        case Y rem X of
            0 -> true;
            _ -> false
        end.

-endif.
