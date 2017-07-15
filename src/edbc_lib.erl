-module(edbc_lib).
-export([post_invariant/2, decreasing_check/3, pre/2, post/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post_invariant/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

decreasing_check(NewValues, OldValues, F) -> 
	case lists:all(
			[NewValue =< OldValue 
			|| {NewValue, OldValue} <- lists:zip(NewValues, OldValues)]) 
	of 
		true -> 
			F();
		false -> 
			error("Decreasing condition does not hold.");
		{false, Msg} -> 
			error("Decreasing condition does not hold." ++ Msg)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pre/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre(Pre, Call) -> 
	case Pre() of 
		true -> 
			Call();
		false -> 
			error("The pre-condition is not hold.");
		{false, Msg} -> 
			error("The pre-condition is not hold." ++ Msg)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Post, Call) ->
	Res =  Call(),
	case Post(Res) of 
		true -> 
			Res;
		false -> 
			error("The post-condition is not hold.");
		{false, Msg} -> 
			error("The post-condition is not hold." ++ Msg)
	end.
