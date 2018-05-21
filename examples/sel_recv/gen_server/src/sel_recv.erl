-module(sel_recv).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-export([test/0]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() -> 
	gen_server:stop({global, ?MODULE}).

test() ->
    gen_server:cast({global, ?MODULE}, test).

init([]) ->
    {ok, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(test, _State) ->
  	io:format("Building test...\n"),
  	List = 
  		[1,2,3,4,5,6,7,8,9],
    lists:map(fun(N) ->
                      gen_server:cast({global, ?MODULE}, {result, N})
              end, lists:reverse(List)),
          	  % end, List),
    {noreply, List};
handle_cast({result, N}, [N|R]) ->
    io:format("result: " ++ integer_to_list(N) ++ "~n"),
    {noreply, R}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.