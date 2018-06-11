-module(edbc_lib).
-export([
			post_invariant/2, 
			decreasing_check/3, 
			sdecreasing_check/3, 
			pre/2, 
			post/2,
			expected_time/2,
			timeout/2,
			is_pure/2,
			spec_check_pre/2,
			spec_check_post/2,
			put_st/0,
			put_call/1,
			put_already_tracing/1
			% sheriff_check/2
		]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post_invariant/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post_invariant(F, {ok, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {ok, State, _}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {noreply, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {noreply, State, _}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {stop, _, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {reply, _, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {reply, _, State, _}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {stop, _, _, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {true, State}) -> 
	correct_message_invariant(F, State);
post_invariant(F, {false, State}) -> 
	correct_message_invariant(F, State);
post_invariant(_, {error, _}) -> 
	true;
post_invariant(_, ignore) -> 
	true.

correct_message_invariant(F, State) ->
	case F(State) of 
		true ->
			true;
		Other ->
			{Other, invariant}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decreasing_check/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decreasing_check(NewValues, OldValues, F) -> 
	decreasing_check_gen(NewValues, OldValues, F, fun(A, B) -> A =< B end).

sdecreasing_check(NewValues, OldValues, F) -> 
	decreasing_check_gen(NewValues, OldValues, F, fun(A, B) -> A < B end).

decreasing_check_gen(NewValues, OldValues, F, CompFun) -> 
	case lists:all(
			fun(B) -> B end,
			[CompFun(NewValue, OldValue) 
			|| {NewValue, OldValue} <- lists:zip(NewValues, OldValues)]) 
	of 
		true -> 
			F();
		false -> 
			[FN | _] = 
				get(edbc_cc),
			ErrorMsg = 
				format(
					"Decreasing condition does not hold."
					" Previous call: ~s."
					" Current call: ~s.",
					[build_call_str([FN | OldValues]), build_call_str([FN | NewValues])]),
			error({ErrorMsg, get(edbc_st)});
		{false, Msg} -> 
			[FN | _] = 
				get(edbc_cc),
			ErrorMsg = 
				format(
					"Decreasing condition does not hold."
					" Previous call: ~s."
					" Current call: ~s."
					" ~s",
					[build_call_str([FN | OldValues]), build_call_str([FN | NewValues]), Msg]),
			error({ErrorMsg, get(edbc_st)})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pre/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre(Pre, Call) -> 
	case Pre() of 
		true -> 
			Call();
		{true, _} -> 
			Call();
		false -> 
			ErrorMsg = 
				format(
					"The precondition does not hold. ~s.",
					[last_call_str()]),
			error({ErrorMsg, get(edbc_st)});
		{false, Msg} -> 
			ErrorMsg = 
				format(
					"The precondition does not hold. ~s. ~s",
					[last_call_str(), Msg]),
			error({ErrorMsg, get(edbc_st)})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Post, Call) ->
	Res =  Call(),
	case Post(Res) of 
		true -> 
			Res;
		{true, _} -> 
			Res;
		{{true, _}, invariant} -> 
			Res;
		{Rep, invariant} ->
			io:format("PETA: ~p\n", [Rep]),
			show_post_report(Rep, Res, "invariant");
		Rep -> 
			show_post_report(Rep, Res, "postcondition")
	end.

show_post_report(Rep, Res, StrPost) ->
	case Rep of 
		false -> 
			ErrorMsg = 
				format(
					"The ~s does not hold. ~s. Result: ~p",
					[StrPost, last_call_str(), Res]),
			error({ErrorMsg, get(edbc_st)});
		{false, Msg} -> 
			ErrorMsg = 
				format(
					"The ~s does not hold. ~s. Result: ~p. ~s",
					[StrPost, last_call_str(), Res, Msg]),
			error({ErrorMsg, get(edbc_st)})
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
		timer:now_diff(os:timestamp(), StartTime)/1000, 
		% 1000 because now_diff returns microseconds and we wants miliseconds
	% io:format("ExeTime: ~p\nExpected: ~p\n", [ExeTime, Expected]),
	case ExeTime < Expected of 
		true -> 
			Res;
		false -> 
			ErrorMsg = 
				format(
					"The execution of ~s"
					" took too much time."
					"Real: ~p ms. Expected: ~p ms. Difference: ~p ms).", 
					[simple_last_call_str(), ExeTime, Expected, ExeTime - Expected]),
			error({ErrorMsg, get_stacktrace()})
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
				format(
					"The execution of ~s" 
					" has been stopped"
					" because it took more time than the expected, i.e. ~p ms.", 
					[simple_last_call_str(), Timeout]),
			error({ErrorMsg, get_stacktrace()})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_check_pre/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec_check_pre(Pre, Call) -> 
	case Pre() of 
		true -> 
			Call();
		false -> 
			ErrorMsg = 
				format(
					"The spec precondition does not hold. ~s.",
					[last_call_str()]),
			error({ErrorMsg, get(edbc_st)});
		{false, Msg} -> 
			ErrorMsg = 
				format(
					"The spec precondition does not hold. ~s. ~s",
					[last_call_str(), Msg]),
			error({ErrorMsg, get(edbc_st)})
	end.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_check_post/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec_check_post(Post, Call) ->
	Res =  Call(),
	case Post(Res) of 
		true -> 
			Res;
		false -> 
			ErrorMsg = 
				format(
					"The spec postcondition does not hold. ~s.",
					[last_call_str()]),
			error({ErrorMsg, get(edbc_st)});
		{false, Msg} -> 
			ErrorMsg = 
				format(
					"The spec postcondition does not hold. ~s. ~s",
					[last_call_str(), Msg]),
			error({ErrorMsg, get(edbc_st)})
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_pure/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(pt_state, 
			{
				pid,
				start_ref,
				end_ref,
				result = none,
				impure_calls_exp = 0,
				last_call
			}).

% This useless clause is defined to unify the interface of all the pre/post functions
is_pure(_, Call) -> 
	is_pure(Call).

is_pure(Call) -> 
	case get(already_tracing) of 
		true -> 
			% TODO: THink a way of trace functions when a tracing process is already running.
			% A solution could be to create a new process an do the tracing there.  
			Call();
		_ -> 
			Self = 
				self(),
			StartRef = 
				make_ref(),
			EndRef = 
				make_ref(),
			Pid = 
				spawn(
					fun() -> 
						edbc_lib:put_already_tracing(true),
						% edbc_lib:receive_start(),
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
						% edbc_lib:send_result(Self, Res, EndRef),
						Self ! {trace, self(), result, Res, EndRef},
						edbc_lib:put_already_tracing(false)
					end), 
			erlang:trace(Pid, true, [call, return_to, set_on_spawn, procs, ports, send, 'receive']),
			erlang:trace_pattern({'_','_','_'}, true, []),
			Pid!{start, StartRef},
			Result = 
				is_pure_tracer(
					#pt_state{
						pid = Pid,
						start_ref = StartRef,
						end_ref = EndRef,
						last_call = get(edbc_cc)
					}),
			case Result of 
				edbc_error ->
					ErrorMsg = 
						format(
							"The function is not pure. ~s.",
							[last_call_str()]),
					error({ErrorMsg, get(edbc_st)});
				{edbc_error, Msg} ->
					ErrorMsg = 
						format(
							"The function is not pure. ~s. ~s",
							[last_call_str(), Msg]),
					error({ErrorMsg, get(edbc_st)});
				{edbc_error_call, {error, Reason}} -> 
					error({Reason, get(edbc_st)});
				{edbc_error_call, {throw, Reason}} -> 
					throw({Reason, get(edbc_st)});
				{edbc_error_call, {exit, Reason}} -> 
					exit({Reason, get(edbc_st)});
				Res -> 
					Res
			end
	end.

% -record(pt_state, 
			% {
			% 	pid,
			% 	start_ref,
			% 	end_ref,
			% 	result = none,
			% 	impure_calls_exp = 0,
			% 	last_call
			% }).

is_pure_tracer(
		State = 
			#pt_state{
				pid = Pid,
				start_ref = StartRef,
				end_ref = EndRef,
				result = Res,
				impure_calls_exp = ImpureFuncExpected,
				last_call = LastCall
			}
	) -> 
	Msg = 
		receive 
			Msg0 ->
				io:format("Msg0: ~p\n", [Msg0]),
				io:format("Pid: ~p\n", [Pid]),
				Msg0
		end,
	case Res of 
		none -> 
			case Msg of 
				{trace, Pid, exit, _} ->
					io:format("RECEIVED EXIT\n"),
					Res;
				% Controls that the start receive is not considered as impure
				{trace, Pid, 'receive', {start, StartRef}} -> 
					is_pure_tracer(State);
				% Controls that the result send is not considered as impure
				{trace, Pid, send, {trace, Pid, result, _, EndRef}, _} ->
					is_pure_tracer(State);
				{trace, Pid, result, FRes, EndRef} -> 
					case Res of 
						none -> 
							is_pure_tracer(State#pt_state{result = FRes});
						_ -> 
							case FRes of 
								{edbc_error_call, _} -> 
									is_pure_tracer(State#pt_state{result = FRes});
								_ -> 
									is_pure_tracer(State)
							end
					end;
				{trace, Pid, call, {M, F, Args}} -> 
					Arity = length(Args),
					case erlang:is_builtin(M, F, Arity) of 
						true -> 
							PureBuiltIns = 
								[{erlang, make_fun, 3}],
							case erl_bifs:is_pure(M, F, Arity) of 
								false -> 
									% io:format("Is not pure: ~p. Tolerance: ~p\n", [{M, F, Arity}, ImpureFuncExpected]),
									case lists:member({M, F, Arity}, PureBuiltIns) of 
										false -> 
											case ImpureFuncExpected of 
												0 -> 
													InfoMsg = 
														format(
															"It has call the unpure BIF ~p:~p/~p"
															" when evaluating ~s.", 
															[M, F, Arity, build_call_str(LastCall)]),
													is_pure_tracer(State#pt_state{result = {edbc_error, InfoMsg}});
												_ ->
													is_pure_tracer(State#pt_state{impure_calls_exp = ImpureFuncExpected - 1})
											end;
										true -> 
											is_pure_tracer(State)
									end;
								true -> 
									is_pure_tracer(#pt_state{last_call = [{M, F} | Args]})
							end;
						false -> 
							InteralImpureFuns = 
								[
									% {MFA, ExpectedImpureOperations}
									{{edbc_lib, put_st, 0}, 3}, 
									{{edbc_lib, put_call, 1}, 1},
									{{edbc_lib, put_already_tracing, 1}, 1}
								],
							case [Expected || {ICall, Expected} <- InteralImpureFuns, {M, F, Arity} == ICall] of 
								[] -> 
									is_pure_tracer(#pt_state{last_call = [{M, F} | Args]});
								[Exp] ->
									% io:format("MODIFY EXP: ~p\n", [Exp]),
									is_pure_tracer(State#pt_state{impure_calls_exp = Exp})
							end
					end;
				{trace, Pid, return_to, _} -> 
					is_pure_tracer(State);
				{trace, Pid, return_from, _, _} -> 
					is_pure_tracer(State);
				Msg -> 
					% io:format("Other: ~p\n", [Msg]),
					InfoMsg = 
						format(
							"It has produced the unpure action ~p when evaluating ~s.", 
							[Msg, build_call_str(LastCall)]),
					is_pure_tracer(State#pt_state{result = {edbc_error, InfoMsg}})
			end;
		_ ->
			% This makes the tracer to ignore further errors when the fisrt one is raised, i.e. only the first error is reported
			case Msg of 
				{trace, Pid, exit, _} ->
					io:format("RECEIVED EXIT\n"),
					Res;
				_ ->
					io:format("Res: ~p\n", [Res]),
					io:format("Pid: ~p\n", [Pid]),
					is_pure_tracer(State)
			end
	end.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Put Info Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_st() ->
	put(edbc_st, get_stacktrace()).

put_call(Args) ->
	put(edbc_cc, Args).

put_already_tracing(Bool) ->
	put(already_tracing, Bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printer functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_stacktrace() ->
	tl(tl(try throw(42) catch 42 -> erlang:get_stacktrace() end)).

build_call_str([{M, F} | Args]) ->
	format(
		"~p:~p(~s)",
		[M, F, string:join(convert_lst_str(Args), ", ")]);
build_call_str([Fun | Args]) ->
	format(
		"~p(~s)",
		[Fun, string:join(convert_lst_str(Args), ", ")]).

convert_lst_str(L) ->
	lists:map(fun convert_str/1,L).

convert_str(E) ->
	format("~p",[E]).

last_call_str() ->
	"Last call: " ++ simple_last_call_str().

simple_last_call_str() ->
	ModCall = 
		case {get(edbc_st), get(edbc_cc)} of 
			{[{Mod,_,_,[]} | _], [F | Args]} -> 
				[{Mod, F} | Args];
			{_, Call} ->
				Call
		end,
	build_call_str(ModCall).

format(Str, Args) -> 			
	lists:flatten(io_lib:format(Str, Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sheriff_call/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This needs to be inlined
% sheriff_check(Value, Type) -> 
% 	case sheriff:check(Value, Type) of
% 		true -> 
% 			true;
% 		false ->
% 			InfoMsg = 
% 				lists:flatten(
% 					io_lib:format(
% 						"The value ~p is not of type ~p\n", 
% 						[Value, Type])),
% 			{false, InfoMsg}
% 	end.

