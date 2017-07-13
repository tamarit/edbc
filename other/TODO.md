# TODO
- http://www.rise4fun.com/Dafny/
- Replace most of the tranformations by function calls to functions (in edbc_lib). For instance the PRE, POST and CALL (all of them are anonymous functions) can be parameters of a function in edbc_lib that would be somethin like:

	case PRE() of 
		true - >
			Result = CALL(),
			case POST(Result) of 
				true -> 
					Result
				false -> 
					...;
				...
			end;
		false -> 
			...
		...
	end
- Add to a gen_server_cpre a dictionary as an internal attribute that for each request stores a sorted list of waiting processes. This could ease the starvation checking.
- Add the previous state to the gen_server_cpre, and make the implementation so return an additional element (a flag) in the returned tuples indicaating whether the previous state should be updated. 
- When there is more than one post or pre join results with lists:and/1
- Generate edoc from contracts.
- Generate eunit tests from contracts.
- Generate property tests from contracts.
- Produce default values instead of an error, i.e. ?PRE(fun.., DefaultValue)
- Invariants when spawning a new process, such as its number of queued messages cannot be greater than 1, etc.
- Introduce locks

- pre in property testing to check that the inputs of a function have always some properties

	prop_calls_to_f() ->
	    ?FORALL({A,B}, {integer(),list(integer())},
		    ?PRE_CALLSTO(
		    	fun m:f/3,
		    	[m:g(A,B), m:h(A), m:i(B,A)],
		    	?P(1) > 3 andalso is_integer(?P(1)) andalso ?P(1) + ?P(2) > ?P(3)
		    )
		).

- decreases  a parameter between calls when recursive
- implementation of the bridge to see how a pre can hold the starvation problems. "Starvation and Critical Race Analyzers for Ada"
- solve problems with invariants 
- Is it needed the old state in the invariant function
- liquid types
- liquid session types
- use some kind of guard that allows to read or not read a message in a gen_server, i.e. as a CPRE
- POST condition that check that the function does not take more than a given time to execute
- The PRE/POST conditions in the gen_server maybe should make the client fail instead of the server.
- The time controling POST could have 2 versions. One that raises an error when the time is passed and another that cut the execution (using an external process) and raises an error. The second version could be very useful for debugging functions generating infinite loops 
- The gen_server_cpre hangs when something fails in the server, specially postconditions.
- Modificar puente para que tenga 4 procesos. Realmente los procesos representan los detectores que hay en cada lado del puente (tanto para la entrada como para la salida).
- Simplify the waiting queues of the readers writers
- Add behaviour info as in https://github.com/uwiger/plain_fsm/blob/master/src/plain_fsm.erl 
