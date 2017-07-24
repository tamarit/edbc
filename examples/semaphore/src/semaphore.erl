-module(semaphore).
-behaviour(gen_server_cpre).

-include_lib("edbc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, cpre/3]).

-export([start/0, initialize/1, acquire/0, release/0, stop/0]).


-record(state, 
	{
		counter = 0,
		top = 0,
		ready = false
	}).


% These are all wrappers for calls to the server
% All the services provided by this server could be perfectly be implemented with casts instead of calls
start() ->
	gen_server_cpre:start_link({local, ?MODULE}, ?MODULE, [], []).
initialize(N) -> 
	gen_server_cpre:call(?MODULE, {initialize, N}, infinity).	
acquire() -> 
	gen_server_cpre:call(?MODULE, acquire, infinity).
release() -> 
	gen_server_cpre:call(?MODULE, release, infinity).
stop() -> 
	gen_server_cpre:stop(?MODULE).

?INVARIANT(fun invariant/1).

invariant(
	State = 
		#state{
			counter = Counter,
			top = Top,
			ready = Ready
		}) -> 
	and_reason(
		[
			{is_integer(Counter), "The state's attribute counter should be an integer."},
			{is_boolean(Ready), "The state's attribute ready should be a boolean."},
			{is_integer(Top), "The state's attribute top should be an integer."},
			{Top >= 0, "The top cannnot be negative."},
			{Counter >= 0, "The counter cannot be negative."},
			{Top >= Counter, "The counter cannot be greater than the top."},
			% (Counter > 0) => (Ready == true).
			{(Counter == 0 orelse Ready), "The semaphore should be ready when counter is greater than 0."}
		]).

and_reason(List) ->  
	lists:foldl(
		fun
			(_, {false, Reason}) ->
				{false, Reason};
			(_, false) -> 
				false;
			({true, _}, true) -> 
				true;
			({false, Reason}, true) -> 
				{false, Reason};
			(B, true) -> 
				B
		end,
		true,
		List).

% This is called when a connection is made to the server
init([]) ->
	{ok, #state{}}.

?PRE(fun() -> 
		#state{counter = Counter, top = Top, ready = Ready} = ?P(3),
		case ?P(1) of 
			{initialize, N} -> 
				{not(Ready), "Semaphore already initialized."};
			acquire -> 
				{Ready, "Semaphore not initialized yet."};
			release -> 
				case Ready of 
					true -> 
						{Counter < Top, "This signal exceeds the semaphore limit."};
					false -> 
						{false, "Semaphore not initialized yet."}
				end
		end
	end).
cpre(acquire, _, State = #state{counter = Counter}) when Counter =< 0 ->
	{
		false, 
		State
	};
cpre(_, _, State) ->
	{
		true, 
		State
	}.


% handle_call is invoked in response to gen_server:call
handle_call({initialize, N}, _, State) ->
	{reply, ok, State#state{top = N, counter = N, ready = true}};
handle_call(acquire, _, State = #state{counter = N}) ->
	{reply, ok, State#state{counter = N - 1}};
handle_call(release, _, State = #state{counter = N}) ->
	{reply, ok, State#state{counter = N + 1}};
handle_call(_Message, _From, State) ->
	% io:format("Error: ~p\n", [_Message]),
	{reply, error, State}.


handle_cast(_Other, State) ->
	{noreply, State}.

handle_info(_Message, State) -> 
	{noreply, State}.

terminate(_Reason, _State) -> 
	ok.

code_change(_OldVersion, State, _Extra) -> 
	{ok, State}.
