-module(readers_writers_test).

-export([test/0]).

test() -> 
	% OutputCompile = 
		% compile:file(readers_writers, [{d,edbc}]),
	% io:format("OutputCompile: ~p\n", [OutputCompile]),
	start(),
	readers_writers:stop(),
	ok.


start() -> 
	Total = 
		100,
	readers_writers:start(),
	Pids = create_readers_writers(Total),
	[Pid!start || Pid <- Pids],
	[
		receive
			finished -> 
				ok
		end
	|| _ <- Pids].


create_readers_writers(N) -> 
	PidsN = 
		create_readers_writers(lists:seq(1, N div 2), r),
	PidsS = 
		create_readers_writers(lists:seq((N div 2) + 1, N), w),
	rearrange_list(PidsN ++ PidsS).

rearrange_list(L) -> 
	[X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

create_readers_writers(Ids, Type) -> 
	Self = self(),
	lists:map(
		fun(Id) -> 
			spawn(
				fun() ->
					case Type of 
						r -> 
							reader(Id, Self);
						w -> 
							writer(Id, Self)
					end
				end)
		end,
		Ids).

reader(Id, Self) ->
	receive 
		start -> 
			timer:sleep(100),
			reader_loop(Id, Self)
	end.

reader_loop(Id, Self) ->
	% io:format("Car ~p entering from ~p\n", [Id, EntryPoint]), 
	pass = 
		readers_writers:request_read(),
	timer:sleep(100),
	readers_writers:finish_read(),
	io:format("Reader ~p finished reading\n", [Id]),
	Self!finished.

writer(Id, Self) -> 
	receive 
		start -> 
			timer:sleep(100),
			writer_loop(Id, Self)
	end.

writer_loop(Id, Self) ->
	% io:format("Car ~p entering from ~p\n", [Id, EntryPoint]), 
	pass = 
		readers_writers:request_write(),
	timer:sleep(100),
	readers_writers:finish_write(),
	io:format("Writer ~p finished writing\n", [Id]),
	Self!finished.

