-module(soter).
-export([label/2,uncoverable/2,error/2,choice/1,choice/2,choice/3,choice/4,choice/5,choice/6]).

label(_,_) -> ok.
label_mail(_,_,_) -> ok.

uncoverable(_,_) -> ok.

error(s,_) -> erlang:error(s).

fun_choice(A) ->
    I = random:uniform(length(A)),
    (lists:nth(I,A))().

choice(A) ->
    I = random:uniform(length(A)),
    lists:nth(I,A).

choice(A,B) -> fun_choice([A,B]).
choice(A,B,C) -> fun_choice([A,B,C]).
choice(A,B,C,D) -> fun_choice([A,B,C,D]).
choice(A,B,C,D,E) -> fun_choice([A,B,C,D,E]).
choice(A,B,C,D,E,F) -> fun_choice([A,B,C,D,E,F]).


%% Could provide basic types as follows
%any_nat() ->
%  choice(fun() -> 0 end, fun() -> 1 + any_nat() end).
