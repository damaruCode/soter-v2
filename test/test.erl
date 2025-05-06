%%  Author: Jonathan
%%  Created: 01/05/2012
%%  Modified: Emanuele 24/5
%%  Description:
%%
%%     Test for soter:choice and assertions
%%

-module(test).
-compile(export_all).

-include_lib("soter.hrl").

-soter_config(peano).

any_nat() ->
    ?SoterChoice(fun()->0 end, fun()-> 1+any_nat() end).

%% Alternative way of defining grammars.
%% Question: is it more readable?!?
?defSoterRule(
n(),
   -> 0,
   -> 1 + n()
).

some_nats() ->
    ?SoterOneOf([1,2,3]).

main() -> test(any_nat()).

test_some() -> test(some_nats()).
test_any() -> test(n()).

test(M) ->
  case M of
      0   -> hurra;
      1   -> ?assert_uncoverable(2), itsone;
      2   -> ?label(two),itstwo;
      3   -> mistery:gazonk(); % used to test unknown calls
      4   -> 'it\'sfour';
      5   -> itsfive;
      7   -> ?soter_error("we do not like 7");
      8   -> itseight;
      9   -> itsnine;
      10  -> ?label(ten),itsten;
      100 -> itshoundred;
      _   -> andtherest
  end.
