-module(list_test).

-compile(export_all).

main() ->
  Y = [id(a), id(b)],
  Z = case Y of
    a -> a; % filtered out at compile time
    {a, b} -> b; % filtered out at compile time
    [a, b] -> c;
    [a, true] -> d
  end,
  Z.

id(X) ->
  X.
