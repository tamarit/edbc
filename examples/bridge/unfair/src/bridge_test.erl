-module(bridge_test).

-export([test/0]).

test() -> 
	% OutputCompile = 
		% compile:file(bridge, [{d,edbc}]),
	% io:format("OutputCompile: ~p\n", [OutputCompile]),
	start(),
	% timer:sleep(10000),
	bridge:stop().


start() -> 
	Total = 
		100,
	bridge:start(),
	Pids = create_sensors(Total),
	[Pid!start || Pid <- Pids],
	[
		receive
			finished -> 
				ok
		end
	|| _ <- Pids].


create_sensors(N) -> 
	PidEntN = 
		create_sensor(N, entry, n),
	PidSalN = 
		create_sensor(N, exit, n),
	PidEntS = 
		create_sensor(N, entry, s),
	PidSalS = 
		create_sensor(N, exit, s),
	rearrange_list([PidEntN, PidSalN, PidEntS, PidSalS]).

rearrange_list(L) -> 
	[X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

create_sensor(N, Type, Place) -> 
	Self = self(),
	spawn(
		fun() -> 
			sensor(N, Type, Place, Self)
		end).

sensor(N, Type, Place, Self) ->
	receive 
		start -> 
			sensor_loop(N, Type, Place, Self)
	end.

sensor_loop(0, _, _, Self) ->
	Self ! finished;
sensor_loop(N, entry, Place, Self) -> 
	Wait = 
		rand:uniform(100) + 100,
	timer:sleep(Wait),
	ok = 
		bridge:warn_arrival(Place),
	pass = 
		bridge:request_enter(Place),
	io:format("A car entered from ~p.\n",[Place]),
	sensor_loop(N - 1, entry, Place, Self);
sensor_loop(N, exit, Place, Self) -> 
	Wait = 
		rand:uniform(100) + 100,
	timer:sleep(Wait),
	case bridge:warn_exit(Place) of
		nosense -> 
			sensor_loop(N, exit, Place, Self);
		ok -> 
			io:format("A car exited. It entered from ~p.\n",[Place]),
			sensor_loop(N - 1, exit, Place, Self)
	end.


