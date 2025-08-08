-module(ecorej).

-export([to_corej/1, to_core/1]).

to_core(Module) ->
  compile:file(Module, ['E']),
  compile:file(Module, ['P']),
  compile:file(Module, ['S']),
  compile:file(Module, [to_core]).

to_corej(Module) ->
  case compile:file(Module, [to_core, binary, no_copt]) of
    {ok, _, Ast} ->
      Json = jsx:encode(enc(Ast)),
      io:put_chars(Json),
      file:write_file(Module ++ ".json", Json),
      erlang:halt(0);
    Err ->
      io:fwrite("ERROR:~n~w", [Err]),
      erlang:halt(1)
  end.

% -record(c_alias, {anno = [] :: list(), var :: cerl:cerl(), pat :: cerl:cerl()}).
enc({c_alias, Anno, Var, Pat}) ->
  #{type => c_alias,
    anno => enc(Anno),
    var => enc(Var),
    pat => enc(Pat)};
% -record(c_apply, {anno = [] :: list(), op :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_apply, Anno, Op, Args}) ->
  #{type => c_apply,
    anno => enc(Anno),
    op => enc(Op),
    args => enc(Args)};
% -record(c_binary, {anno = [] :: list(), segments :: [cerl:c_bitstr()]}).
enc({c_binary, Anno, Segments}) ->
  #{type => c_binary,
    anno => enc(Anno),
    segments => enc(Segments)};
% -record(c_bitstr,
%     {anno = [] :: list(),
%     val :: cerl:cerl(),
%     size :: cerl:cerl(),
%     unit :: cerl:cerl(),
%     type :: cerl:cerl(),
%     flags :: cerl:cerl()}).
enc({c_bitstr, Anno, Val, Size, Unit, Type, Flags}) ->
  #{type => c_bitstr,
    anno => enc(Anno),
    val => enc(Val),
    size => enc(Size),
    unit => enc(Unit),
    bitstr_type => enc(Type),
    flags => enc(Flags)};
% -record(c_call,
%     {anno = [] :: list(), module :: cerl:cerl(), name :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_call, Anno, Module, Name, Args}) ->
  #{type => c_call,
    anno => enc(Anno),
    module => enc(Module),
    name => enc(Name),
    args => enc(Args)};
% -record(c_case, {anno = [] :: list(), arg :: cerl:cerl(), clauses :: [cerl:cerl()]}).
enc({c_case, Anno, Arg, Clauses}) ->
  #{type => c_case,
    anno => enc(Anno),
    arg => enc(Arg),
    clauses => enc(Clauses)};
% -record(c_catch, {anno = [] :: list(), body :: cerl:cerl()}).
enc({c_catch, Anno, Body}) ->
  #{type => c_catch,
    anno => enc(Anno),
    body => enc(Body)};
% -record(c_clause,
%     {anno = [] :: list(),
%     pats :: [cerl:cerl()],
%     guard :: cerl:cerl(),
%     body :: cerl:cerl() | any()}).
enc({c_clause, Anno, Pats, Guard, Body}) ->
  #{type => c_clause,
    anno => enc(Anno),
    pats => enc(Pats),
    guard => enc(Guard),
    body => enc(Body)};
% -record(c_cons, {anno = [] :: list(), hd :: cerl:cerl(), tl :: cerl:cerl()}).
enc({c_cons, Anno, Hd, Tl}) ->
  #{type => c_cons,
    anno => enc(Anno),
    hd => enc(Hd),
    tl => enc(Tl)};
% -record(c_fun, {anno = [] :: list(), vars :: [cerl:cerl()], body :: cerl:cerl()}).
enc({c_fun, Anno, Vars, Body}) ->
  #{type => c_fun,
    anno => enc(Anno),
    vars => enc(Vars),
    body => enc(Body)};
% -record(c_let,
%     {anno = [] :: list(), vars :: [cerl:cerl()], arg :: cerl:cerl(), body :: cerl:cerl()}).
enc({c_let, Anno, Vars, Arg, Body}) ->
  #{type => c_let,
    anno => enc(Anno),
    vars => enc(Vars),
    arg => enc(Arg),
    body => enc(Body)};
% -record(c_letrec,
%     {anno = [] :: list(), defs :: [{cerl:cerl(), cerl:cerl()}], body :: cerl:cerl()}).
enc({c_letrec, Anno, Defs, Body}) ->
  #{type => c_letrec,
    anno => enc(Anno),
    defs => enc(Defs),
    body => enc(Body)};
% -record(c_literal, {anno = [] :: list(), val :: any()}).
enc({c_literal, Anno, Val}) ->
  #{type => c_literal,
    anno => enc(Anno),
    val => enc(Val)};
% -record(c_map,
%     {anno = [] :: list(),
%     arg = #c_literal{val = #{}} :: cerl:c_var() | cerl:c_literal(),
%     es :: [cerl:c_map_pair()],
%     is_pat = false :: boolean()}).
enc({c_map, Anno, Arg, Es, IsPat}) ->
  #{type => c_map,
    anno => enc(Anno),
    arg => enc(Arg),
    es => enc(Es),
    is_pat => IsPat};
% -record(c_map_pair,
%     {anno = [] :: list(),
%     op :: #c_literal{val :: assoc} | #c_literal{val :: exact},
%     key :: any(),
%     val :: any()}).
enc({c_map_pair, Anno, Op, Key, Val}) ->
  #{type => c_map_pair,
    anno => enc(Anno),
    op => enc(Op),
    key => enc(Key),
    val => enc(Val)};
% -record(c_module,
%     {anno = [] :: list(),
%     name :: cerl:cerl(),
%     exports :: [cerl:cerl()],
%     attrs :: [{cerl:cerl(), cerl:cerl()}],
%     defs :: [{cerl:cerl(), cerl:cerl()}]}).
enc({c_module, Anno, Name, Exports, Attrs, Defs}) ->
  #{type => c_module,
    anno => enc(Anno),
    name => enc(Name),
    exports => enc(Exports),
    attrs => enc(Attrs),
    defs => enc(Defs)};
% -record(c_opaque, {anno = [] :: list(), val :: any()}).
enc({c_opaque, Anno, Val}) ->
  #{type => c_opaque,
    anno => enc(Anno),
    val => enc(Val)};
% -record(c_primop, {anno = [] :: list(), name :: cerl:cerl(), args :: [cerl:cerl()]}).
enc({c_primop, Anno, Name, Args}) ->
  #{type => c_primop,
    anno => enc(Anno),
    name => enc(Name),
    args => enc(Args)};
% -record(c_receive,
%     {anno = [] :: list(),
%     clauses :: [cerl:cerl()],
%     timeout :: cerl:cerl(),
%     action :: cerl:cerl()}).
enc({c_receive, Anno, Clauses, Timeout, Action}) ->
  #{type => c_receive,
    anno => enc(Anno),
    clauses => enc(Clauses),
    timeout => enc(Timeout),
    action => enc(Action)};
% -record(c_seq,
%     {anno = [] :: list(),
%     arg :: cerl:cerl() | any(),
%     body :: cerl:cerl()}).
enc({c_seq, Anno, Arg, Body}) ->
  #{type => c_seq,
    anno => enc(Anno),
    arg => enc(Arg),
    body => enc(Body)};
% -record(c_try,
%     {anno = [] :: list(),
%     arg :: cerl:cerl(),
%     vars :: [cerl:cerl()],
%     body :: cerl:cerl(),
%     evars :: [cerl:cerl()],
%     handler :: cerl:cerl()}).
enc({c_try, Anno, Arg, Vars, Body, EVars, Handler}) ->
  #{type => c_try,
    anno => enc(Anno),
    arg => enc(Arg),
    vars => enc(Vars),
    body => enc(Body),
    evars => enc(EVars),
    handler => enc(Handler)};
% -record(c_tuple, {anno = [] :: list(), es :: [cerl:cerl()]}).
enc({c_tuple, Anno, Es}) ->
  #{type => c_tuple,
    anno => enc(Anno),
    es => enc(Es)};
% -record(c_values, {anno = [] :: list(), es :: [cerl:cerl()]}).
enc({c_values, Anno, Es}) ->
  #{type => c_values,
    anno => enc(Anno),
    es => enc(Es)};
% -record(c_var, {anno = [] :: list(), name :: cerl:var_name()}).
enc({c_var, Anno, Name}) ->
  #{type => c_var,
    anno => enc(Anno),
    name => enc(Name)};
enc(Tuple) when is_tuple(Tuple) ->
  [enc(Elem) || Elem <- tuple_to_list(Tuple)];
enc(List) when is_list(List) ->
  [enc(Elem) || Elem <- List];
enc(Other) ->
  Other.
