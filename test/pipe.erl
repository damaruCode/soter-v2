%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     A simple sequence of N processes arranged in a linear pipe:
%%     every process knows only the next in the row and forwards everything to it.
%%
%%     A property Soter can prove is that the mailbox of each pipe_node
%%     contains at most 1 message at any given time.
%%
%%     Adapted from [Kobayashi, Nakade and Yonezawa, 1995]


-module(pipe).
-compile(export_all).

-soter_config(peano).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

-uncoverable("mail>=2").


main()->
    Head = init_pipe(fun(X)-> X+1 end, ?any_nat(),self()),
    Head ! 0,
    sink().

ring(N)->
    Head = init_pipe(fun(X)-> X+1 end, N,self()),
    Head ! 0,
    pipe_node(fun(X)-> io:fwrite("~w~n",[X]), X end, Head).


pipe_node(Fun, Next) ->
    ?label_mail('mail',self()),
    receive
        Msg -> Next ! Fun(Msg),
               pipe_node(Fun, Next)
    end.

init_pipe( _ , 0 , Next) -> ?label('pipe_ready'), Next;
init_pipe(Fun, N, Next) ->
    New = spawn(fun()-> pipe_node(Fun, Next) end),
    init_pipe(Fun, N-1, New).

sink() ->
    receive X -> X end.

%%% To use with in the interpreter
tryme(N)->
    Head = init_pipe(fun(X)->X+1 end,N,self()),
    Head ! 0,
    sink().

% peano(0)->zero;
% peano(N)->{s,peano(N-1)}.

