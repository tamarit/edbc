-module(library).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
% The file includes some modifications. Please see the original file here: http://20bits.com/article/erlang-a-generic-server-tutorial
-behaviour(gen_server).

-include_lib("edbc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, checkout/2, lookup/1, return/1, stop/0]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	
lookup(Book) -> gen_server:call(?MODULE, {lookup, Book}).
return(Book) -> gen_server:call(?MODULE, {return, Book}).
stop() -> gen_server:stop(?MODULE).

% This is called when a connection is made to the server
init([]) ->
	Library = dict:new(),
	{ok, Library}.
	% {ok, []}.


% handle_call is invoked in response to gen_server:call
handle_call({checkout, Who, Book}, _From, Library) ->
	Response = case dict:is_key(Book, Library) of
		true ->
			NewLibrary = Library,
			{already_checked_out, Book};
		false ->
			NewLibrary = dict:append(Book, Who, Library),
			ok
	end,
	{reply, Response, NewLibrary};

handle_call({lookup, Book}, _From, Library) ->
	Response = case dict:is_key(Book, Library) of
		true ->
			{who, lists:nth(1, dict:fetch(Book, Library))};
		false ->
			{not_checked_out, Book}
	end,
	{reply, Response, Library};
	% {reply, Response, []};

handle_call({return, Book}, _From, Library) ->
	NewLibrary = dict:erase(Book, Library),
	{reply, ok, NewLibrary};

handle_call(_Message, _From, Library) ->
	{reply, error, Library}.
% ?POST(fun post_handle_call/0).

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Library) -> 
	{noreply, Library}.
handle_info(_Message, Library) -> 
	{noreply, Library}.
terminate(_Reason, _Library) -> 
	ok.
code_change(_OldVersion, Library, _Extra) -> 
	{ok, Library}.


% Added functions

is_dict(DictCand) ->
  is_tuple(DictCand) andalso element(1, DictCand) =:= dict.

% post_handle_call() -> 
% 	is_dict(element(3, ?R)).

?INVARIANT(fun invariant/1).

invariant(State) -> 
	is_dict(State).
