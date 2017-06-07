% c(bridge, [{d, edbc}]).

-module(bridge).
-behaviour(gen_server).

-include_lib("edbc.hrl").

-define(THRESHOLD(Total), Total div 5).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1, request_enter/1, warn_exit/0, stop/0]).

% These are all wrappers for calls to the server
start(Total) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Total], []).
request_enter(EntryPoint) -> 
	gen_server:call(?MODULE, {request_enter, EntryPoint}).	
warn_exit() -> 
	% io:format("LLEGA\n"),
	% gen_server:call(?MODULE, warn_exit).
	gen_server:cast(?MODULE, warn_exit).
stop() -> 
	gen_server:stop(?MODULE).

?INVARIANT(fun invariant/1).

invariant({Passing, Waiting, Total}) -> 
		is_integer(Passing)
	andalso
		is_integer(Total)
	andalso
		is_list(Waiting)
	% andalso
	% 	length(Waiting) =< ?THRESHOLD(Total)
	.

% This is called when a connection is made to the server
init([Total]) ->
	{ok, {0, [], Total}}.


% handle_call is invoked in response to gen_server:call
handle_call({request_enter, EntryPoint}, {From, _}, {Passing, Waiting, Total}) ->
	{Reply, {NPassing, NWaiting}} = 
		case Passing of 
			0 -> 
				case EntryPoint of 
					n -> 
						{pass, {1, Waiting -- [{From, n}]}};
					s -> 
						{pass, {-1, Waiting -- [{From, s}]}}
				end;
			N when is_integer(N), N > 0 -> 
				case EntryPoint of 
					n -> 
						{pass, {Passing + 1, Waiting -- [{From, n}]}};
					s -> 
						{wait, {Passing, [{From, s} | (Waiting -- [{From, n}])] }}
				end;
			N when is_integer(N), N < 0 -> 
				case EntryPoint of 
					n -> 
						{wait, {Passing, [{From, n} | (Waiting -- [{From, n}])]}};
					s -> 
						{pass, {Passing - 1, Waiting -- [{From, s}]}}
				end
		end,
	% io:format("{Reply, NState}: ~p\n", [{Reply, NState}]),
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

