-module(bridge_fair_cpre_test).

-export([test/0]).

test() -> 
	% OutputCompile = 
		compile:file(bridge_fair_cpre, [{d,edbc}]),
	% io:format("OutputCompile: ~p\n", [OutputCompile]),
	start(),
	% timer:sleep(10000),
	bridge_fair:stop().


start() -> 
	Total = 
		100,
	bridge_fair:start(Total),
	Pids = create_cars(Total),
	[Pid!start || Pid <- Pids],
	[
		receive
			finished -> 
				ok
		end
	|| _ <- Pids].


create_cars(N) -> 
	PidsN = 
		create_cars(lists:seq(1, N div 2), n),
	PidsS = 
		create_cars(lists:seq((N div 2) + 1, N), s),
	rearrange_list(PidsN ++ PidsS).

rearrange_list(L) -> 
	[X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

create_cars(Ids, EntryPoint) -> 
	Self = self(),
	lists:map(
		fun(Id) -> 
			spawn(
				fun() -> 
					put(parent, Self),
					car(Id, EntryPoint)
				end)
		end,
		Ids).

car(Id, EntryPoint) ->
	receive 
		start -> 
			timer:sleep(100),
			car_loop(Id, EntryPoint)
	end.

car_loop(Id, EntryPoint) ->
	% io:format("Car ~p entering from ~p\n", [Id, EntryPoint]), 
	Answer = 
		bridge_fair:request_enter(EntryPoint),
	case Answer of 
		wait ->
			% io:format("Car ~p wait for entering from ~p\n", [Id, EntryPoint]),
			timer:sleep(100), 
			car_loop(Id, EntryPoint);
		pass -> 
			timer:sleep(100),
			bridge_fair:warn_exit(),
			io:format("Car ~p passed entering from ~p\n", [Id, EntryPoint]),
			% car_loop(Id, EntryPoint)
			get(parent)!finished
	end.


