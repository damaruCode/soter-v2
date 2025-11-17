%%  Author: Jonathan Kochems
%%  Created: 11/August/2012
%%  Description:
%%
%%      A slight generalisation of the Erlang implementation
%%      of the Accumulator Factory in [Rosetta Code](http://rosettacode.org/wiki/Accumulator_factory).
%%      The factory creates a function that has is stateful and
%%      which combines its arguments with its current state.
%%      In this example the factory is parametrised by a function
%%      that produces the new state; we instantiate it with an
%%      arbitrary function of arity two which produces an arbitrary
%%      natural number. We check for the boundedness of the state's mailbox
%%      and that there are no conflicting requests to update the state.
%%

-module(state_factory).
-include_lib("soter.hrl").
-include_lib("grammars.hrl").
-compile(export_all).

-soter_config(peano).

-uncoverable("state_mail > 1").
-uncoverable("after_receive > 0, state_mail > 0").

state(N, NewState) ->
    ?label_mail("state_mail"),
    receive
        {P,In}->
            ?label("after_receive"),
            M = NewState(N,In),
            P ! M,
            state(M, NewState)
    end.

factory(N, NewState) ->
    P=spawn(fun() -> state(N,NewState) end),
    fun(In)->
            P!{self(),In},
            receive
                Out-> Out
            end
    end.

main() ->
    FunWithState = factory(?any_nat(), fun(_X,_Y) -> ?any_nat() end),
    call_loop(?any_nat(), FunWithState).

call_loop(1,Fun) -> Fun(?any_nat());
call_loop(N,Fun) -> Fun(?any_nat()),
                  call_loop(N-1,Fun).

