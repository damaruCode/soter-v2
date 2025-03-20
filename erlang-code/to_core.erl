-module(to_core).

-export([g/0]).

g() ->
  case compile:file("fib.erl", [to_core, binary, no_copt]) of
    {ok, _M, Ast} ->
      io:put_chars(Json = json:encode(Ast, fun(X, E) -> enc(X, E) end)),
      file:write_file("fib.json", Json),
      erlang:halt(0);
    Err ->
      io:fwrite("ERROR:~n~w", [Err]),
      erlang:halt(1)
  end.

% -record(c_module,
%     {anno = [] :: list(),
%     name :: cerl:cerl(),
%     exports :: [cerl:cerl()],
%     attrs :: [{cerl:cerl(), cerl:cerl()}],
%     defs :: [{cerl:cerl(), cerl:cerl()}]}).
enc({c_module, Anno, Name, Exports, Attrs, Defs}, _E) ->
  json:encode_map(#{<<"type">> => <<"c_module">>,
                    <<"anno">> => Anno,
                    <<"args">> =>
                      #{<<"name">> => Name,
                        <<"exports">> => Exports,
                        <<"attrs">> => Attrs,
                        <<"defs">> => Defs}},
                  fun(X1, E1) -> enc(X1, E1) end);
% -record(c_alias,
%     {anno = [] :: list(),
%     var :: cerl:cerl(),
%     pat :: cerl:cerl()}).
enc({c_alias, Anno, Var, Pat}, _E) ->
  json:encode_map(#{<<"type">> => c_alias,
                    <<"anno">> => Anno,
                    <<"args">> => #{<<"var">> => Var, <<"pat">> => Pat}},
                  fun(X1, E1) -> enc(X1, E1) end);
% -record(c_literal,
%     {anno = [] :: list(),
%     val :: any()}).
enc({c_literal, Anno, Val}, _E) ->
  json:encode_map(#{<<"type">> => c_literal,
                    <<"anno">> => Anno,
                    <<"args">> => #{<<"val">> => Val}},
                  fun(X1, E1) -> enc(X1, E1) end);
enc(X, _E) when is_tuple(X) ->
  json:encode_value(tuple_to_list(X), fun(X1, E1) -> enc(X1, E1) end);
enc(X, _E) when is_list(X) ->
  try
    Y = list_to_binary(X),
    json:encode_binary(Y)
  catch
    error:badarg ->
      json:encode_list(X, fun(X1, E1) -> enc(X1, E1) end)
  end;
enc(X, E) ->
  json:encode_value(X, E).
