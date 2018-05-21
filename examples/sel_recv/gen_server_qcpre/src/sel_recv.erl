-module(sel_recv).
-behaviour(gen_server_cpre).

-include_lib("edbc.hrl").

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
      cpre/3,
     terminate/2, code_change/3]).

-export([test/0]).

start_link() ->
    gen_server_cpre:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
	gen_server_cpre:stop(?MODULE).

test() ->
  gen_server_cpre:call(?MODULE, test).

init([]) ->
    {ok, 0}.

cpre({result, N}, _, State = [N|R]) ->
  {true, State};
cpre({result, N}, _, State) ->
  {false, State};
cpre(test, _, State) ->
  {true, State}.

% cpre(_, _, State) ->
%   {true, State}.


% handle_call(_Request, _From, State) ->
%     Reply = ok,
%     {reply, Reply, State}.

handle_call(test, _From, _State) ->
  	% io:format("Building test...\n"),
  	List = 
  		[1,2,3,4,5,6,7,8,9],
    lists:map(fun(N) ->
                      spawn(
                        fun() -> 
                          gen_server_cpre:call(?MODULE, {result, N})
                        end)
              end, lists:reverse(List)),
    {reply, ok, List};
handle_call({result, N}, _From, [N|R]) ->
    io:format("result: " ++ integer_to_list(N) ++ "~n"),
    {reply, ok, R}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.