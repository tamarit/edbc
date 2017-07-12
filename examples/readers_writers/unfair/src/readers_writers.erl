-module(readers_writers).
-behaviour(gen_server_cpre).

-include_lib("edbc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, cpre/3]).

-export([start/0, request_read/0, finish_read/0, request_write/0, finish_write/0, stop/0]).

-record(state, 
	{
		readers = 0,
		writer = false,
		waiting = [],
		prev_state = none
	}).

% These are all wrappers for calls to the server
start() ->
	gen_server_cpre:start_link({local, ?MODULE}, ?MODULE, [], []).
request_read() -> 
	gen_server_cpre:call(?MODULE, request_read).	
finish_read() -> 
	gen_server_cpre:cast(?MODULE, finish_read).
request_write() -> 
	gen_server_cpre:call(?MODULE, request_write).	
finish_write() -> 
	gen_server_cpre:cast(?MODULE, finish_write).
stop() -> 
	gen_server_cpre:stop(?MODULE).

?INVARIANT(fun invariant/1).

invariant(
	State = 
	#state{
		readers = Readers, 
		writer = Writer, 
		waiting = Waiting, 
		prev_state = PrevState}
	) -> 
	% io:format("State: ~p\n" ,[State]),
		is_integer(Readers)
	andalso
		Readers >= 0
	andalso 
		is_boolean(Writer)
	andalso
		is_list(Waiting)
	andalso
		% Common invariant in readers-writers problem
		((not Writer) orelse Readers == 0)
	% andalso
	% 	case PrevState of 
	% 		none -> 
	% 			true; 
	% 		_ -> 
	% 				case length(Waiting) > PrevState#state.waiting of 
	% 					false -> 
	% 						true;
	% 					true -> 
	% 							length(Waiting) == 0 
	% 						orelse
	% 							Readers =< PrevState#state.readers
	% 						% true
	% 				end
	% 			andalso
	% 				is_substr(Waiting, PrevState#state.waiting)
	% 				% true
	% 	end
	.


is_substr([H | L1], [H | L2]) -> 
	is_substr(L1, L2);
is_substr([_,H | L1], [H | L2]) -> 
	is_substr(L1, L2);
is_substr([H | L1], [_,H | L2]) -> 
	is_substr(L1, L2);
is_substr([_ | _], [_ | _]) -> 
	false;
is_substr(_, []) -> 
	true;
is_substr([], _) -> 
	true.


% This is called when a connection is made to the server
init([]) ->
	{ok, #state{}}.

add_to_waiting(State = #state{waiting = Waiting}, Item) -> 
	% case Waiting of 
	% 	[Item | _] -> 
	% 		State;
	% 	_ -> 
			case lists:member(Item, Waiting) of 
				true -> 
					State;
				false -> 
					NWaiting = 
						% case Waiting of 
						% 	[] -> 
						% 		[{Type, From, State}];
						% 	_ -> 
								Waiting ++ [Item]
						% end
						,
					State#state{waiting = NWaiting}
			end
			.
	% end.
remove_from_waiting(State = #state{waiting = Waiting}, Item) ->
	% case Waiting of 
	% 	[{Type, From} | TWaiting] -> 
	% 		NWaiting = 
	% 			case  TWaiting of 
	% 			 	[{WType, WFrom} | TTWaiting] -> 
	% 			 		[{WType, WFrom, State} | TTWaiting];
	% 			 	[] -> 
	% 			 		[]
	% 			end
	% 		State#state{waiting = NWaiting};
	% 	_ -> 
			State#state{waiting = Waiting -- [Item]}
	% end
	.

cpre(Req, From , State) -> 
	Res = 
		cpre_actual(Req, From, State),
	% io:format("cpre(~p, ~p, ~p) = ~p\n", [Req, From , State, Res]),
	Res.


cpre_actual(request_read, From, State = #state{writer = false, waiting = []}) ->
	{
		true, 
		update_prev_state(State, remove_from_waiting(State, {r, From}))
	};
cpre_actual(request_read, From, State = #state{writer = false, waiting = [{r, From} | _]}) ->
	{
		true,
		update_prev_state(State, remove_from_waiting(State, {r, From}))
	};
cpre_actual(request_read, From, State = #state{writer = false}) ->
	{
		false,
		update_prev_state(State, add_to_waiting(State, {r, From}))
	};
cpre_actual(request_read, From, State = #state{writer = true}) ->
	{
		false, 
		update_prev_state(State, add_to_waiting(State, {r, From}))
	};
cpre_actual(request_write, From, State = #state{readers = 0, waiting = []}) ->
	{
		true, 
		update_prev_state(State, remove_from_waiting(State, {w, From}))
	};
cpre_actual(request_write, From, State = #state{readers = 0, waiting = [{w, From} | _]}) ->
	{
		true,
		update_prev_state(State, remove_from_waiting(State, {w, From}))
	};
cpre_actual(request_write, From, State = #state{readers = 0}) ->
	{
		false,
		update_prev_state(State, add_to_waiting(State, {w, From}))
	};
cpre_actual(request_write, From, State) ->
	{
		false, 
		update_prev_state(State, add_to_waiting(State, {w, From}))
	};
cpre_actual(_, _, _) ->
	true.


% handle_call is invoked in response to gen_server:call
handle_call(request_read, _, State) ->
	{Reply, NState} = 
		{
			pass, 
			State#state{
				readers = State#state.readers + 1
			}
		},
	{reply, Reply, update_prev_state(State, NState)};
handle_call(request_write, _, State) ->
	{Reply, NState} = 
		{
			pass, 
			State#state{
				writer = true
			}
		},
	{reply, Reply, update_prev_state(State, NState)};
handle_call(_Message, _From, State) ->
	% io:format("Error: ~p\n", [_Message]),
	{reply, error, State}.


% We get compile warnings from gen_server unless we define these
handle_cast(finish_read, State) ->	
	NState = 
		State#state{
			readers = State#state.readers - 1
		},
	{noreply, update_prev_state(State, NState)};
handle_cast(_Other, State) ->
	NState = 
		State#state{
			writer = false
		},
	{noreply, update_prev_state(State, NState)}.

update_prev_state(State, NState) -> 
	NState#state{
		prev_state = State#state{prev_state = none} % To avoid create a useless big structure
	}.

handle_info(_Message, Library) -> 
	{noreply, Library}.
terminate(_Reason, _Library) -> 
	ok.
code_change(_OldVersion, Library, _Extra) -> 
	{ok, Library}.

