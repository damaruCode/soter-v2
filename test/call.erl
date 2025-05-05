-module(call).

-export([use_fib/1]).

use_fib(N) ->
  function:fib(N).
