
-define(SoterOneOf(A), soter:choice(A)).
-define(SoterChoice, soter:choice).

-define(defSoterRule(H,A,B), H -> soter:choice(fun()A end, fun() B end)).
-define(defSoterRule(H,A,B,C), H -> soter:choice(fun()A end, fun() B end, fun() C end)).
-define(defSoterRule(H,A,B,C,D), H -> soter:choice(fun()A end, fun() B end, fun() C end, fun() D end)).
-define(defSoterRule(H,A,B,C,D,E), H -> soter:choice(fun()A end, fun() B end, fun() C end, fun() D end, fun() E end)).
-define(defSoterRule(H,A,B,C,D,E,F), H -> soter:choice(fun()A end, fun() B end, fun() C end, fun() D end, fun() E end, fun() F end)).
% etc...

-define(soter_assert(E), ?soter_assert(??E, E) ).
-define(soter_assert(S, E), case (E) of true -> ok; false -> ?soter_error(S) end ).

-ifdef(SOTER).

-define(soter_error(X), soter:error(X,?LINE)).
-define(label(Name), soter:label(Name,?LINE)).
-define(label_mail(Name), (fun(X)->soter:label_mail(Name,X,?LINE) end)(self())).
-define(label_mail(Name,V), (fun(X)->soter:label_mail(Name,X,?LINE) end)(V)).
-define(assert_uncoverable(), soter:uncoverable(1,?LINE)).
-define(assert_uncoverable(X), soter:uncoverable(X,?LINE)).

-else.

-define(soter_error(X), erlang:error(X)).
-define(label(Name), ok).
-define(label_mail(Name), ok).
-define(label_mail(Name,V), ok).
-define(assert_uncoverable(), ok).
-define(assert_uncoverable(X), ok).

-endif.

