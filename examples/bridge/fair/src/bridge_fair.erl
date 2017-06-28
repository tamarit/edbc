% c(bridge_fair, [{d, edbc}]).

-module(bridge_fair).
-behaviour(gen_server).

-include_lib("edbc.hrl").

-define(THRESHOLD(Total), Total div 2).
% -define(THRESHOLD_LIMIT(Total), Total div 20).
-define(LIMIT_TIMES_SIZE, 2).
-define(LIMIT_TIMES_SIZE_TO_STOP, 1.2).
-define(LIMIT_PASS, 5).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1, request_enter/1, warn_exit/0, stop/0]).

% These are all wrappers for calls to the server
start(Total) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Total], []).
request_enter(EntryPoint) -> 
	gen_server:call(?MODULE, {request_enter, EntryPoint}).	
warn_exit() -> 
	% io:format("LLEGA\n"),
	% gen_server:cast(?MODULE, warn_exit).
	gen_server:cast(?MODULE, warn_exit).
stop() -> 
	gen_server:stop(?MODULE).

?INVARIANT(fun invariant/1).

invariant({Passing, Waiting, Total}) -> 
	% io:format("ARRIBA: ~p\n", [{Passing, Waiting, Total}]),
	Res = 
			is_integer(Passing)
		andalso
			is_integer(Total)
		andalso
			is_list(Waiting)
		andalso
			not(critical_waiting(Waiting, Total, ?LIMIT_TIMES_SIZE)),
	% io:format("ACABA: ~p\n", [Res]),
	% io:format("NS: ~p\n", [check_limit(n, s, Waiting, ?LIMIT_TIMES_SIZE)]),
	% io:format("SN: ~p\n", [check_limit(s, n, Waiting, ?LIMIT_TIMES_SIZE)]),
	Res.

check_limit(EntryA, EntryB, Waiting, Limit) -> 
	waiting(Waiting, EntryA) > (Limit * waiting(Waiting, EntryB)).

waiting(Waiting, EntryPoint) -> 
	length([EntryPoint || {_, EntryPointW} <- Waiting, EntryPointW == EntryPoint]).

critical_waiting(Waiting, Total, Limit) -> 
	(		check_limit(n, s, Waiting, Limit)
		orelse
			check_limit(s, n, Waiting, Limit)
	)
	andalso
		(waiting(Waiting, n) + waiting(Waiting, s)) >= ?THRESHOLD(Total).

who_is_critical(Waiting) -> 
	case check_limit(n, s, Waiting, ?LIMIT_TIMES_SIZE_TO_STOP) of 
		true -> 
			n;
		false -> 
			s
	end.


% This is called when a connection is made to the server
init([Total]) ->
	{ok, {0, [], Total}}.


% handle_call is invoked in response to gen_server:call
handle_call({request_enter, EntryPoint}, {From, _}, {Passing, Waiting, Total}) ->
	% io:format("{request_enter, EntryPoint}: ~p\n", [{request_enter, EntryPoint}]),
	{Reply, {NPassing, NWaiting}} = 
		case Passing of 
			0 -> 
				case critical_waiting(Waiting, Total, ?LIMIT_TIMES_SIZE_TO_STOP) of 
					true -> 
						case who_is_critical(Waiting) of 
							EntryPoint -> 
								{pass, {ns2int(EntryPoint), Waiting -- [{From, EntryPoint}]}};
							_ -> 
								{wait, {0, [{From, EntryPoint} | (Waiting -- [{From, EntryPoint}])] }}
						end;
					false -> 
						{pass, {ns2int(EntryPoint), Waiting -- [{From, EntryPoint}]}}
				end;
			N when is_integer(N), N > 0, abs(N) < ?LIMIT_PASS ->
				FunPassOrWait = 
					fun() -> 
						case EntryPoint of 
							n -> 
								{pass, {Passing + 1, Waiting -- [{From, n}]}};
							s -> 
								{wait, {Passing, [{From, s} | (Waiting -- [{From, s}])] }}
						end
					end, 
				case critical_waiting(Waiting, Total, ?LIMIT_TIMES_SIZE_TO_STOP) of 
					true -> 
						{wait, {Passing, [{From, EntryPoint} | (Waiting -- [{From, EntryPoint}])] }};
					false -> 
						FunPassOrWait()
				end;
			N when is_integer(N), N < 0, abs(N) < ?LIMIT_PASS -> 
				FunPassOrWait = 
					fun() -> 
						case EntryPoint of 
							n -> 
								{wait, {Passing, [{From, n} | (Waiting -- [{From, n}])]}};
							s -> 
								{pass, {Passing - 1, Waiting -- [{From, s}]}}
						end
					end,
				case critical_waiting(Waiting, Total, ?LIMIT_TIMES_SIZE_TO_STOP) of 
					true -> 
						{wait, {Passing, [{From, EntryPoint} | (Waiting -- [{From, EntryPoint}])] }};
					false -> 
						FunPassOrWait()
				end;
			N when is_integer(N), abs(N) == ?LIMIT_PASS -> 
				{wait, {Passing, [{From, EntryPoint} | (Waiting -- [{From, EntryPoint}])] }}
		end,
	% io:format("{Reply, {NPassing, NWaiting}}: ~p\n", [{Reply, {NPassing, NWaiting}}]),
	{reply, Reply, {NPassing, NWaiting, Total}};

% handle_call(warn_exit, From, {Passing, Waiting}) ->
% 	NPassing = 
% 		case Passing < 0 of 
% 			true -> 
% 				Passing + 1;
% 			false -> 
% 				Passing - 1
% 		end,
% 	% io:format("NPassing: ~p\n", [NPassing]),
% 	{reply, ok, {NPassing, Waiting}};

handle_call(_Message, _From, State) ->
	% io:format("Error: ~p\n", [_Message]),
	{reply, error, State}.
% ?POST(fun post_handle_call/0).

% We get compile warnings from gen_server unless we define these
% handle_cast(_Message, Library) -> 
% 	{noreply, Library}.
handle_cast(warn_exit, {Passing, Waiting, Total}) ->	
	NPassing = 
		case Passing < 0 of 
			true -> 
				Passing + 1;
			false -> 
				Passing - 1
		end,
	% io:format("NPassing: ~p\n", [NPassing]),
	{noreply, {NPassing, Waiting, Total}};
handle_cast(_Other, State) ->
	{noreply, State}.

handle_info(_Message, Library) -> 
	{noreply, Library}.
terminate(_Reason, _Library) -> 
	ok.
code_change(_OldVersion, Library, _Extra) -> 
	{ok, Library}.

ns2int(n) -> 1;
ns2int(s) -> -1.

