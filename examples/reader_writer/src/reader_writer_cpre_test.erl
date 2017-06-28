-module(reader_writer_cpre_test).

-export([test/0]).

test() -> 
	% OutputCompile = 
		% compile:file(reader_writer_cpre, [{d,edbc}]),
	% io:format("OutputCompile: ~p\n", [OutputCompile]),
	start(),
	reader_writer_cpre:stop(),
	ok.


start() -> 
	Total = 
		10,
	reader_writer_cpre:start(),
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
		create_reader_writer(lists:seq(1, N div 2), r),
	PidsS = 
		create_reader_writer(lists:seq((N div 2) + 1, N), w),
	rearrange_list(PidsN ++ PidsS).

rearrange_list(L) -> 
	[X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

create_reader_writer(Ids, Type) -> 
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
		reader_writer_cpre:request_read(),
	timer:sleep(100),
	reader_writer_cpre:finish_read(),
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
		reader_writer_cpre:request_write(),
	timer:sleep(100),
	reader_writer_cpre:finish_write(),
	io:format("Writer ~p finished writing\n", [Id]),
	Self!finished.

