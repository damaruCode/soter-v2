%%% In order to verify that soter_error() is not called
%%% a refinement needs to be applied (see line 47)

-module(ufirewall).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

main() -> start(?list_of_peanos()).

start(S) ->
	Srv = spawn(fun() -> server(fun(X)->pred(X)end) end),
	Fir = spawn(fun() -> server(firewall(Srv,fun(X)-> notZero(X) end)) end),
	sendall(Fir, S),
	ok.

sendall(To, []) -> ok; %To ! {ack, self()};
sendall(To, [X|Xs]) -> To ! {call, self(), X},
					   sendall(To, Xs).


%% property to be verified: unreachability of erlang:error()
pred(zero) -> ?soter_error("pred called with zero");
pred({s,N}) -> N.

notZero(zero) -> false;
notZero({s,_}) -> true.

server(HandleCall)->
	receive
		{call, From, Par} -> From ! {answer, self(), HandleCall(Par)}
	end,
	server(HandleCall).

% to verify prop, X needs to be unfolded!
firewall(Proc, Test) ->
	fun(Y) ->
	  case Y of
	  {s,X} ->
		case Test({s,X}) of
			true  -> Proc ! {call, self(), {s,X}},
					 receive
					 	{answer, Proc, Res} -> Res
					 end;
			false -> bad_req;
	  (zero) ->
		case Test(zero) of
			true  -> Proc ! {call, self(), zero},
					 receive
					 	{answer, Proc, Res} -> Res
					 end;
			false -> bad_req
		end
	  end
    end
end.
