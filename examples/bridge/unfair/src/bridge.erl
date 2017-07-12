-module(bridge).
-behaviour(gen_server_cpre).

-include_lib("edbc.hrl").

-define(THRESHOLD(Total), Total div 20).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, cpre/3]).

-export([start/0, request_enter/1, warn_arrival/1, warn_exit/1, stop/0]).


-record(state, 
	{
		passing = 0, % Negative indicates that the cars are passing from N to S. Possitive from S to N.
		waitingN = false,
		waitingS = false,
		prev_state = none
	}).


% These are all wrappers for calls to the server
start() ->
	gen_server_cpre:start_link({local, ?MODULE}, ?MODULE, [], []).
request_enter(EntryPoint) -> 
	gen_server_cpre:call(?MODULE, {request_enter, EntryPoint}, infinity).	
warn_arrival(ExitPoint) -> 
	gen_server_cpre:call(?MODULE, {warn_arrival, ExitPoint}, infinity).
warn_exit(ExitPoint) -> 
	gen_server_cpre:call(?MODULE, {warn_exit, ExitPoint}, infinity).
stop() -> 
	gen_server_cpre:stop(?MODULE).

?INVARIANT(fun invariant_starvation/1).

invariant(
	State = 
	#state{
		passing = Passing, 
		waitingN = WaitingN,
		waitingS = WaitingS,
		prev_state = PrevState
	}) -> 
	% io:format("State: ~p\n", [State]),
		is_integer(Passing)
	andalso
		is_boolean(WaitingN)
	andalso
		is_boolean(WaitingS)
	andalso
		case PrevState of 
			#state{} -> 
				true;
			none -> 
				true;
			_ ->
				false 
		end
	.

invariant_starvation(
	State = 
	#state{
		passing = Passing, 
		waitingN = WaitingN,
		waitingS = WaitingS,
		prev_state = PrevState
	}) -> 
	% io:format("State: ~p\n", [State]),
		is_integer(Passing)
	andalso
		is_boolean(WaitingN)
	andalso
		is_boolean(WaitingS)
	andalso
		case PrevState of 
			#state{} -> 
				true;
			none -> 
				true;
			_ ->
				{false, "The state term is not the expected."}
		end
	% Starvation condition	
	andalso
		case PrevState of 
			#state{} ->
				% If the state change corresponds to a car that passed.
				case abs(Passing) > abs(PrevState#state.passing) of 
					true -> 
						case {PrevState#state.waitingN, PrevState#state.waitingS} of 
							{true, true} -> 
								case PrevState#state.passing == 0 of 
									true -> 
										true;
									false -> 
										% If both are waiting it means that the direction that was being used have stopped
										{
											false, 
											lists:flatten(
												io_lib:format(
													"There were cars waiting on both sides. A priority should be given to the cars that wanted to enter from the opposite side of the current way.\n"
													++ "State info\nCars waiting N (previous state): ~p\nCars waiting S (previous state): ~p\nPrevious passing: ~p\n Current passing: ~p\n", 
													[PrevState#state.waitingN, PrevState#state.waitingS, PrevState#state.passing, State#state.passing]))
										}
								end; 
							{true, false} -> 
								case Passing < 0 of 
									true -> 
										true;
									false -> 
										% If there where only cars waiting at N then they should pass.
										{
											false, 
											lists:flatten(
												io_lib:format(
													"There were cars waiting on the north side. They should pass.\n"
													++ "State info\nCars waiting N (previous state): ~p\nCars waiting S (previous state): ~p\nPrevious passing: ~p\n Current passing: ~p\n", 
													[PrevState#state.waitingN, PrevState#state.waitingS, PrevState#state.passing, State#state.passing]))
										}
								end; 
							{false, true} -> 
								case Passing > 0 of 
									true -> 
										true;
									false -> 
										% If there where only cars waiting at S then they should pass.
										{
											false, 
											lists:flatten(
												io_lib:format(
													"There were cars waiting on the south side. They should pass.\n"
													++ "State info\nCars waiting N (previous state): ~p\nCars waiting S (previous state): ~p\nPrevious passing: ~p\n Current passing: ~p\n", 
													[PrevState#state.waitingN, PrevState#state.waitingS, PrevState#state.passing, State#state.passing]))
										}
								end; 
							{false, false} -> 
								%It is not possible for a car to pass without previously being waiting 
								{false, "A car passed without be previously waiting."} 
						end;
					false -> 
						true
				end;
			_ -> 
				true
		end
	.

% This is called when a connection is made to the server
init([]) ->
	{ok, #state{}}.


cpre(A, B, C) -> 
	Res = cpre_(A, B, C),
	case Res of 
		{false, _} -> 
			% io:format("cpre: ~p\n", [{{A, B, C}, Res}]);
			ok;
		_ -> 
			ok 
	end,
	Res.


% cpre(_, _) -> 
% 	true.
cpre_({request_enter, s}, _, State = #state{passing = Passing}) when Passing < 0 ->
	{
		false,
		State
	};
cpre_({request_enter, n}, _, State = #state{passing = Passing}) when Passing > 0 ->
	{
		false, 
		State
	};
% cpre({request_enter, s}, _, State = #state{passing = Passing}) when Passing > 0 ->
% 	{
% 		true, 
% 		State
% 	};
% cpre({request_enter, n}, _, State = #state{passing = Passing}) when Passing < 0 ->
% 	{
% 		true, 
% 		State
% 	};
cpre_(_, _, State) ->
	{
		true, 
		State
	}.


% handle_call is invoked in response to gen_server:call
handle_call({request_enter, EntryPoint}, _, State) ->
	{Reply, NState} = 
		{pass, pass(State, EntryPoint)},
	% io:format("NState RE: ~p\n", [{EntryPoint, NState#state.passing, _Form}]),
	{reply, Reply, update_prev_state(State, NState)};
handle_call({warn_arrival, EntryPoint}, _, State) ->
	{Reply, NState} = 
		{ok, wait(State, EntryPoint)},
	{reply, Reply, update_prev_state(State, NState)};
handle_call({warn_exit, EntryPoint}, _, State) ->
	{Reply, NState} = 
		exit_car(State, EntryPoint),
	% io:format("NState WE: ~p\n", [{_From, EntryPoint, Reply, NState#state.passing}]),
	{reply, Reply, NState};
handle_call(_Message, _From, State) ->
	% io:format("Error: ~p\n", [_Message]),
	{reply, error, State}.


handle_cast(_Other, State) ->
	{noreply, State}.

handle_info(_Message, Library) -> 
	{noreply, Library}.
terminate(_Reason, _Library) -> 
	ok.
code_change(_OldVersion, Library, _Extra) -> 
	{ok, Library}.

update_prev_state(State, NState) -> 
	NState#state{
		prev_state = State#state{prev_state = none} % To avoid create a useless big structure
	}.

wait(State, n) -> 
	State#state{waitingN = true};
wait(State, s) -> 
	State#state{waitingS = true}.

pass(State = #state{passing = Passing}, n) -> 
	State#state{passing = Passing - 1, waitingN = false};
pass(State = #state{passing = Passing}, s) -> 
	State#state{passing = Passing + 1, waitingS = false}.

exit_car(State, n) -> 
	exit_car(
		State, 
		fun(Passing) -> Passing < 0 end,
		fun(Passing) -> Passing + 1 end);
exit_car(State, s) -> 
	exit_car(
		State, 
		fun(Passing) -> Passing > 0 end,
		fun(Passing) -> Passing - 1 end).

exit_car(State = #state{passing = Passing}, FunCompare, FunUpdate) -> 
	case FunCompare(Passing) of 
		true -> 
			{ok, update_prev_state(State, State#state{passing = FunUpdate(Passing)})};
		false -> 
			{nosense, State}
	end.