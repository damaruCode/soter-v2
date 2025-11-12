%%  Author: Emanuele
%%  Created: 2012
%%  Description:
%%
%%     A firewall process protects another process (implementing a "partial function")
%%     from bad requests.
%%
%%     In order to verify that soter:error() is not called
%%     a refinement needs to be applied (see line 47 and ufirewall.erl)
%%

-module(firewall).
-compile(export_all).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

main() -> start(?list_of_peanos()).

start(S) ->
	Srv = spawn(fun() -> server(fun(X)->pred(X)end) end),
	Fir = spawn(fun() -> server(firewall(Srv,fun(X)-> notZero(X) end)) end),
	sendall(Fir, S),
	%collectall(),    % unnecessary
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
		%{ack, From} -> From ! {ack, self()};
		{call, From, Par} -> From ! {answer, self(), HandleCall(Par)}
	end,
	server(HandleCall).

firewall(Proc, Test) ->
	fun(X) ->
	% to verify prop, X needs to be unfolded!
		case Test(X) of
			true  -> Proc ! {call, self(), X},
					 receive
					 	{answer, Proc, Res} -> Res
					 end;
			false -> bad_req
		end
	end.
