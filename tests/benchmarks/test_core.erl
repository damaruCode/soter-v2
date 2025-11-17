%% Author: bordaigorl
%% Created: 01/mar/2012
%% Description:
%%  Just a dummy module to test how erlc translates some code

-module(test_core).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

main() -> {spawn(fun()->ok end),?list_of_peanos(),make_ref()}.

main2() ->
  start(27).

start(N) ->
    M = case N of
          27 -> N;
          3 -> 5 ! 5
        end,
    test(N,N).


test(X,Y) ->
  receive
    {X,Y,Y,P} -> {X,P}
  end.

jude(L)->
    spawn(test_core, test, L).
