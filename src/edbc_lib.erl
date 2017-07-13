-module(edbc_lib).
-export([post_invariant/2, decreasing_check/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post_invariant/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Outputs of the functions modifying the state in gen_server_cpre
% ===============================================================

% {ok,State}
% {ok,State,_}
% {noreply, NewState}
% {noreply, NewState,_}
% {stop, Reason, NewState}
% {reply, Reply, NewState}
% {reply, Reply, NewState,_}
% {reply, Reply, NewState,_}
% {stop, Reason, Reply, NewState}
% {true, State}
% {false, State}
% {error, Reason}
% ignore

post_invariant(F, {ok, State}) -> 
	F(State);
post_invariant(F, {ok, State, _}) -> 
	F(State);
post_invariant(F, {noreply, State}) -> 
	F(State);
post_invariant(F, {noreply, State, _}) -> 
	F(State);
post_invariant(F, {stop, _, State}) -> 
	F(State);
post_invariant(F, {reply, _, State}) -> 
	F(State);
post_invariant(F, {reply, _, State, _}) -> 
	F(State);
post_invariant(F, {stop, _, _, State}) -> 
	F(State);
post_invariant(F, {true, State}) -> 
	F(State);
post_invariant(F, {false, State}) -> 
	F(State);
post_invariant(_, {error, _}) -> 
	true;
post_invariant(_, ignore) -> 
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decreasing_check/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decreasing_check(NewValue, OldValue, F) 
		when NewValue =< OldValue -> 
	F();
decreasing_check(NewValue, OldValue, _) -> 
	ErrorMsg = 
		lists:flatten(
			io_lib:format(
					"Decreasing condition does not hold.\n"
				 	"Previous value: ~p\nNew Value: ~p\n", 
				[OldValue, NewValue])),
	error(ErrorMsg).

