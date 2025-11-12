%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     Very simple test program.
%%     This cannot be proven error-free with the current match function.
%%

-module(match2_eeehhh).

-compile(export_all).

-include_lib("soter.hrl").

-soter_config(peano).

g(X) ->
  % Y = {s, X},
  case {s, X} of
    {s, a} ->
      ?label(branch2);
    {s, b} ->
      ?label(branch1)
  end,
  {bla, X}.

main() ->
  g(a),
  g(b).
