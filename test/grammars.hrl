%%% Useful nondeterministic generators

%%% Do not forget to include soter.hrl!
%%% -include_lib("soter.hrl").

%% Grammar for numbers in "human" form:
% any_nat() -> ?SoterChoice(fun()-> 0 end, fun()-> any_nat()+1 end).

% any_peano() -> ?SoterChoice(fun()-> zero end, fun()-> {s, any_peano()} end).

% list_of_nats() -> list_of(fun()->any_nat() end).
% list_of_peanos() -> list_of(fun()->any_peano() end).

% list_of(F) -> ?SoterChoice(fun()-> [] end, fun()-> [F() | list_of(F)] end).

% any_bool() -> ?SoterOneOf([true,false]).


%%% These are the corresponding macros.
-define(any_nat(), (?any_nat)()).
-define(any_nat, ((fun()->
        RES = fun(F) -> fun() ->
            ?SoterChoice(fun()-> 0 end, fun()-> (F(F))()+1 end)
        end end,
        RES(RES)
     end)())).

-define(any_peano(),(?any_peano)()).
-define(any_peano, ((fun()->
        RES = fun(F) -> fun() ->
            ?SoterChoice(fun()-> zero end, fun()-> {s, (F(F))()} end)
        end end,
        RES(RES)
    end)())).

-define(any_bool(),(?any_bool)()).
-define(any_bool, (fun()-> ?SoterOneOf([true,false]) end)).

-define(list_of(G), ((fun()->
        RES = fun(F) -> fun() ->
            ?SoterChoice(fun()-> [] end, fun()-> [(G)() | (F(F))()] end)
        end end,
        RES(RES)
    end)())).

-define(list_of_nats(), (?list_of_nats)()).
-define(list_of_nats, ?list_of(?any_nat)).
-define(list_of_peanos(), (?list_of_peanos)()).
-define(list_of_peanos, ?list_of(?any_peano)).
