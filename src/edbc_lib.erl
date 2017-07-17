-module(edbc_lib).
-export([
			post_invariant/2, 
			decreasing_check/3, 
			pre/2, 
			post/2,
			expected_time/2,
			timeout/2,
			is_pure/2
		]).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expected_time/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expected_time(Time, Call) -> 
	Expected = 
		Time(),
	StartTime = 
		os:timestamp(),
	Res = Call(),
	ExeTime = 
		timer:now_diff(os:timestamp(), StartTime)/1000, % 1000 because now_diff returns microseconds and we wants miliseconds
	% io:format("ExeTime: ~p\nExpected: ~p\n", [ExeTime, Expected]),
	case ExeTime < Expected of 
		true -> 
			Res;
		false -> 
			ErrorMsg = 
				lists:flatten(
					io_lib:format(
						"The execution of the function took too much time\nReal: ~p ms\nExpected: ~p ms\nDifference: ~p ms).\n", 
						[ExeTime, Expected, ExeTime - Expected])),
			error(ErrorMsg)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% timeout/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timeout(Time, Call) -> 
	Timeout = 
		Time(),
	Self = 
		self(),
	MsgRef = 
		make_ref(),
	spawn(fun() -> Self!{Call(), MsgRef} end),
	receive
		{Res, MsgRef} -> 
			Res
	after 
		Timeout -> 
			ErrorMsg = 
				lists:flatten(
					io_lib:format(
						"The execution of the function took more time than the expected, i.e. ~p ms.\n", 
						[Timeout])),
			error(ErrorMsg)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_pure/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To unify the interface of all the pre/post functions
is_pure(_, Call) -> 
	is_pure(Call).

is_pure(Call) -> 
	Self = 
		self(),
	StartRef = 
		make_ref(),
	EndRef = 
		make_ref(),
	Pid = 
		spawn(
			fun() -> 
				receive 
					{start, StartRef} -> 
						ok
				end,
				Res = 
					try Call() of
						Res0 ->
							Res0
					catch
						E1:E2  ->
							{edbc_error_call, {E1, E2}}
					end,
				Self ! {Res, EndRef}
			end),
	erlang:trace(Pid, true, [call, set_on_spawn, procs, ports, send, 'receive']),
	erlang:trace_pattern({'_','_','_'}, true, []),
	Pid!{start, StartRef},
	Result = 
		is_pure_tracer(Pid, StartRef, EndRef, none),
	case Result of 
		edbc_error ->
			error("The function is not pure.");
		{edbc_error, Msg} ->
			error("The function is not pure." ++ Msg);
		{edbc_error_call, {error, Reason}} -> 
			error(Reason);
		{edbc_error_call, {throw, Reason}} -> 
			throw(Reason);
		{edbc_error_call, {exit, Reason}} -> 
			exit(Reason);
		Res -> 
			Res
	end.

is_pure_tracer(Pid, StartRef, EndRef, Res) -> 
	receive 
		{trace, Pid, exit, _} ->
			Res;
		{trace, Pid, 'receive', {start, StartRef}} -> 
			is_pure_tracer(Pid, StartRef, EndRef, Res);
		{trace, Pid, send, {_, EndRef}, _} ->
			is_pure_tracer(Pid, StartRef, EndRef, Res);
		{FRes, EndRef} -> 
			case Res of 
				none -> 
					is_pure_tracer(Pid, StartRef, EndRef, FRes);
				_ -> 
					case FRes of 
						{edbc_error_call, _} -> 
							is_pure_tracer(Pid, StartRef, EndRef, FRes);
						_ -> 
							is_pure_tracer(Pid, StartRef, EndRef, Res)
					end
			end;
		{trace, Pid, call, {M, F, Args}} -> 
			Arity = length(Args),
			case erlang:is_builtin(M, F, Arity) of 
				true -> 
					case erl_bifs:is_pure(M, F, Arity) of 
						false -> 
							InfoMsg = 
								lists:flatten(
									io_lib:format(
										"It has call the unpure BIF ~p:~p/~p", 
										[M, F, Arity])),
							is_pure_tracer(Pid, StartRef, EndRef, {edbc_error, InfoMsg});
						true -> 
							is_pure_tracer(Pid, StartRef, EndRef, Res)
					end;
				false -> 
					is_pure_tracer(Pid, StartRef, EndRef, Res)
			end;
		{trace, Pid, return_to, _} -> 
			is_pure_tracer(Pid, StartRef, EndRef, Res);
		{trace, Pid, return_from, _, _} -> 
			is_pure_tracer(Pid, StartRef, EndRef, Res);
		Msg -> 
			% io:format("Other: ~p\n", [_Msg]),
			InfoMsg = 
				lists:flatten(
					io_lib:format(
						"It has produced the unpure action ~p\n", 
						[Msg])),
			is_pure_tracer(Pid, StartRef, EndRef, {edbc_error, InfoMsg})
	end.
