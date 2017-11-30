-module(semaphore_tests).

-compile(export_all).

test() -> 
	[
		begin 
			io:format(
				"\n~s\nTest with ~p\n~s\n",
				[sep(), T, sep()]),
			(fun ?MODULE:T/0)(),
			io:format("\n")
		end
	|| 
		T <- all_tests()
	].

sep() -> 
	"******************************".

all_tests() ->
	[
		ini,
		double_ini,
		acquire_before_ini,
		release_before_ini,
		extra_release,
		should_wait,
		scenario
	].

ok() -> 
	io:format("OK").

fail() -> 
	fail("").

fail(Reason) -> 
	io:format("FAIL " ++ Reason).

ini() -> 
	semaphore:start(),
	case semaphore:initialize(3) of 
		ok -> 
			ok();
		_ -> 
			fail()
	end,
	semaphore:stop().

double_ini() -> 
	semaphore:start(), 
	try
		semaphore:initialize(3),
		semaphore:initialize(3),
		fail(),
		semaphore:stop()
	catch
		_:_ -> 
			ok()
	end. 

acquire_before_ini() -> 
	semaphore:start(), 
	try
		semaphore:acquire(),
		fail(),
		semaphore:stop()
	catch
		_:_ -> 
			ok()
	end. 

release_before_ini() -> 
	semaphore:start(), 
	try
		semaphore:release(),
		fail(),
		semaphore:stop()
	catch
		_:_ -> 
			ok()
	end. 

extra_release() -> 
	semaphore:start(), 
	try
		semaphore:initialize(3),
		semaphore:release(),
		fail(),
		semaphore:stop()
	catch
		_:_ -> 
			ok()
	end. 

should_wait() -> 
	semaphore:start(),
	semaphore:initialize(3),
	Self = self(),
	FunTest = 
		fun() -> 
			semaphore:acquire(),
			semaphore:acquire(),
			semaphore:acquire(),
			Self!semaphore:acquire()
		end,
	spawn(FunTest),
	receive
		ok -> 
			fail()
	after 
		100 -> 
			ok()
	end,
	semaphore:stop(). 	

scenario() -> 
	semaphore:start(),
	N = 3,
	Instances = 6,
	semaphore:initialize(N),
	Self = self(),
	Counter = 
		spawn(fun () -> counter(0, N, Self) end),
	FunTest = 
		fun() -> 
			semaphore:acquire(),
			Counter!enter,
			timer:sleep(100),
			Counter!exit,
			semaphore:release(),
			Self!finish
		end,
	[spawn(FunTest) || _ <- lists:seq(1, Instances)],
	Res = 
		receive_data(Instances, Counter),
	case Res of 
		error -> 
			fail("There were more processes inside the critical section than expected");
		0 -> 
			ok();
		_ -> 
			fail("There are some process that remain inside the critical section.")
	end,
	semaphore:stop(). 	

counter(N, Top, Parent) when N > Top -> 
	Parent!error;
counter(N, Top, Parent) when N =< Top -> 
	receive
		enter -> 
			counter(N + 1, Top, Parent);
		exit -> 
			counter(N - 1, Top, Parent);
		finish -> 
			Parent!{current, N}
	end.

receive_data(0, Counter) -> 
	Counter!finish,
	receive
		{current, N} -> 
			N
	end;
receive_data(Pending, Counter) -> 
	receive
		error -> 
			error;
		finish -> 
			receive_data(Pending - 1, Counter)
	end.



