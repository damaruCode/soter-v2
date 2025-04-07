-module(ecorej).

-export([to_corej/1]).

to_corej(Module) ->
  case compile:file(Module, [to_core, binary, no_copt]) of
    {ok, _, Ast} ->
      io:put_chars(Json = json:encode(Ast, fun(X, E) -> enc(X, E) end)),
      file:write_file("core.json", Json),
      erlang:halt(0);
    Err ->
      io:fwrite("ERROR:~n~w", [Err]),
      erlang:halt(1)
  end.

% -record(c_alias, {anno = [] :: list(), var :: cerl:cerl(), pat :: cerl:cerl()}).
enc({c_alias, Anno, Var, Pat}, _E) ->
  json:encode_map(#{<<"type">> => c_alias,
                    <<"anno">> => Anno,
                    <<"var">> => Var,
                    <<"pat">> => Pat},
                  fun enc/2);
% -record(c_apply, {anno = [] :: list(), op :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_apply, Anno, Op, Args}, _E) ->
  json:encode_map(#{<<"type">> => c_apply,
                    <<"anno">> => Anno,
                    <<"op">> => Op,
                    <<"args">> => Args},
                  fun enc/2);
% -record(c_binary, {anno = [] :: list(), segments :: [cerl:c_bitstr()]}).
enc({c_binary, Anno, Segments}, _E) ->
  json:encode_map(#{<<"type">> => c_binary,
                    <<"anno">> => Anno,
                    <<"segments">> => Segments},
                  fun enc/2);
% -record(c_bitstr,
%     {anno = [] :: list(),
%     val :: cerl:cerl(),
%     size :: cerl:cerl(),
%     unit :: cerl:cerl(),
%     type :: cerl:cerl(),
%     flags :: cerl:cerl()}).
enc({c_bitstr, Anno, Val, Size, Unit, Type, Flags}, _E) ->
  json:encode_map(#{<<"type">> => c_bitstr,
                    <<"anno">> => Anno,
                    <<"val">> => Val,
                    <<"size">> => Size,
                    <<"unit">> => Unit,
                    <<"type">> => Type,
                    <<"flags">> => Flags},
                  fun enc/2);
% -record(c_call,
%     {anno = [] :: list(), module :: cerl:cerl(), name :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_call, Anno, Module, Name, Args}, _E) ->
  json:encode_map(#{<<"type">> => c_call,
                    <<"anno">> => Anno,
                    <<"module">> => Module,
                    <<"name">> => Name,
                    <<"args">> => Args},
                  fun enc/2);
% -record(c_case, {anno = [] :: list(), arg :: cerl:cerl(), clauses :: [cerl:cerl()]}).
enc({c_case, Anno, Arg, Clauses}, _E) ->
  json:encode_map(#{<<"type">> => c_case,
                    <<"anno">> => Anno,
                    <<"arg">> => Arg,
                    <<"clauses">> => Clauses},
                  fun enc/2);
% -record(c_catch, {anno = [] :: list(), body :: cerl:cerl()}).
enc({c_catch, Anno, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_catch,
                    <<"anno">> => Anno,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_clause,
%     {anno = [] :: list(),
%     pats :: [cerl:cerl()],
%     guard :: cerl:cerl(),
%     body :: cerl:cerl() | any()}).
enc({c_clause, Anno, Pats, Guard, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_clause,
                    <<"anno">> => Anno,
                    <<"pats">> => Pats,
                    <<"guard">> => Guard,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_cons, {anno = [] :: list(), hd :: cerl:cerl(), tl :: cerl:cerl()}).
enc({c_cons, Anno, Hd, Tl}, _E) ->
  json:encode_map(#{<<"type">> => c_cons,
                    <<"anno">> => Anno,
                    <<"hd">> => Hd,
                    <<"tl">> => Tl},
                  fun enc/2);
% -record(c_fun, {anno = [] :: list(), vars :: [cerl:cerl()], body :: cerl:cerl()}).
enc({c_fun, Anno, Vars, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_fun,
                    <<"anno">> => Anno,
                    <<"vars">> => Vars,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_let,
%     {anno = [] :: list(), vars :: [cerl:cerl()], arg :: cerl:cerl(), body :: cerl:cerl()}).
enc({c_let, Anno, Vars, Arg, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_let,
                    <<"anno">> => Anno,
                    <<"vars">> => Vars,
                    <<"arg">> => Arg,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_letrec,
%     {anno = [] :: list(), defs :: [{cerl:cerl(), cerl:cerl()}], body :: cerl:cerl()}).
enc({c_letrec, Anno, Defs, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_letrec,
                    <<"anno">> => Anno,
                    <<"defs">> => Defs,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_literal, {anno = [] :: list(), val :: any()}).
enc({c_literal, Anno, Val}, _E) ->
  json:encode_map(#{<<"type">> => c_literal,
                    <<"anno">> => Anno,
                    <<"val">> => Val},
                  fun enc/2);
% -record(c_map,
%     {anno = [] :: list(),
%     arg = #c_literal{val = #{}} :: cerl:c_var() | cerl:c_literal(),
%     es :: [cerl:c_map_pair()],
%     is_pat = false :: boolean()}).
enc({c_map, Anno, Arg, Es, IsPat}, _E) ->
  json:encode_map(#{<<"type">> => c_map,
                    <<"anno">> => Anno,
                    <<"arg">> => Arg,
                    <<"es">> => Es,
                    <<"is_pat">> => IsPat},
                  fun enc/2);
% -record(c_map_pair,
%     {anno = [] :: list(),
%     op :: #c_literal{val :: assoc} | #c_literal{val :: exact},
%     key :: any(),
%     val :: any()}).
enc({c_map_pair, Anno, Op, Key, Val}, _E) ->
  json:encode_map(#{<<"type">> => c_map_pair,
                    <<"anno">> => Anno,
                    <<"op">> => Op,
                    <<"key">> => Key,
                    <<"val">> => Val},
                  fun enc/2);
% -record(c_module,
%     {anno = [] :: list(),
%     name :: cerl:cerl(),
%     exports :: [cerl:cerl()],
%     attrs :: [{cerl:cerl(), cerl:cerl()}],
%     defs :: [{cerl:cerl(), cerl:cerl()}]}).
enc({c_module, Anno, Name, Exports, Attrs, Defs}, _E) ->
  json:encode_map(#{<<"type">> => c_module,
                    <<"anno">> => Anno,
                    <<"name">> => Name,
                    <<"exports">> => Exports,
                    <<"attrs">> => Attrs,
                    <<"defs">> => Defs},
                  fun enc/2);
% -record(c_opaque, {anno = [] :: list(), val :: any()}).
enc({c_opaque, Anno, Val}, _E) ->
  json:encode_map(#{<<"type">> => c_opaque,
                    <<"anno">> => Anno,
                    <<"val">> => Val},
                  fun enc/2);
% -record(c_primop, {anno = [] :: list(), name :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_primop, Anno, Name, Args}, _E) ->
  json:encode_map(#{<<"type">> => c_primop,
                    <<"anno">> => Anno,
                    <<"name">> => Name,
                    <<"args">> => Args},
                  fun enc/2);
% -record(c_receive,
%     {anno = [] :: list(),
%     clauses :: [cerl:cerl()],
%     timeout :: cerl:cerl(),
%     action :: cerl:cerl()}).
enc({c_receive, Anno, Clauses, Timeout, Action}, _E) ->
  json:encode_map(#{<<"type">> => c_receive,
                    <<"anno">> => Anno,
                    <<"clauses">> => Clauses,
                    <<"timeout">> => Timeout,
                    <<"action">> => Action},
                  fun enc/2);
% -record(c_seq,
%     {anno = [] :: list(),
%     arg :: cerl:cerl() | any(),
%     body :: cerl:cerl()}).
enc({c_seq, Anno, Arg, Body}, _E) ->
  json:encode_map(#{<<"type">> => c_seq,
                    <<"anno">> => Anno,
                    <<"arg">> => Arg,
                    <<"body">> => Body},
                  fun enc/2);
% -record(c_try,
%     {anno = [] :: list(),
%     arg :: cerl:cerl(),
%     vars :: [cerl:cerl()],
%     body :: cerl:cerl(),
%     evars :: [cerl:cerl()],
%     handler :: cerl:cerl()}).
enc({c_try, Anno, Arg, Vars, Body, EVars, Handler}, _E) ->
  json:encode_map(#{<<"type">> => c_try,
                    <<"anno">> => Anno,
                    <<"arg">> => Arg,
                    <<"vars">> => Vars,
                    <<"body">> => Body,
                    <<"evars">> => EVars,
                    <<"handler">> => Handler},
                  fun enc/2);
% -record(c_tuple, {anno = [] :: list(), es :: [cerl:cerl()]}).
enc({c_tuple, Anno, Es}, _E) ->
  json:encode_map(#{<<"type">> => c_tuple,
                    <<"anno">> => Anno,
                    <<"es">> => Es},
                  fun enc/2);
% -record(c_values, {anno = [] :: list(), es :: [cerl:cerl()]}).
enc({c_values, Anno, Es}, _E) ->
  json:encode_map(#{<<"type">> => c_values,
                    <<"anno">> => Anno,
                    <<"es">> => Es},
                  fun enc/2);
% -record(c_var, {anno = [] :: list(), name :: cerl:var_name()}).
enc({c_var, Anno, Name}, _E) ->
  json:encode_map(#{<<"type">> => c_var,
                    <<"anno">> => Anno,
                    <<"name">> => Name},
                  fun enc/2);
enc(X, _E) when is_tuple(X) ->
  json:encode_value(tuple_to_list(X), fun enc/2);
enc(X, _E) when is_list(X) ->
  case X of
    [] ->
      json:encode_list([], fun enc/2);
    _ ->
      try
        Y = list_to_binary(X),
        json:encode_binary(Y)
      catch
        error:badarg ->
          json:encode_list(X, fun enc/2)
      end
  end;
enc(X, E) ->
  json:encode_value(X, E).
