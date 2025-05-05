-module(try_catch).

-export([use_fib/1]).

use_fib(N) ->
  try
    function:fib(N)
  catch
    error:Error ->
      {error, Error}
  end.
