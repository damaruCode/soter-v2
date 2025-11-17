-module(huch).
-compile(export_all).

-include_lib("soter.hrl").

main() -> S = spawn(1).

setupc(S) -> spawn(fun()-> c(S) end), setupc(S).

s() -> receive P -> P!a end, s().

c(S) -> S ! self(),
       receive
           a -> receive
                    a -> error
                end
       end.
