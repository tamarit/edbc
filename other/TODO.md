# TODO

PRE, POST, ...
==============

- Replace most of the tranformations by function calls to functions (in edbc_lib). For instance the PRE, POST and CALL (all of them are anonymous functions) can be parameters of a function in edbc_lib that would be somethin like:
- When there is more than one post or pre join results with lists:and/1

```
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
```

- Produce default values instead of an error, i.e. ?PRE(fun.., DefaultValue)
- Produce a personalized error message for PREs y POSTs, i.e. ?PRE(fun.., ErrorMsg)
- Last two extensions are mutually exclusive. If there is a default value, then there is not sens for an error message, and viceversa.
- decreases for a recursions a -> b -> a -> b
- Allow to integreate PREs, POSTs, y DECREASEs
- POST condition that check that the function does not take more than a given time to execute
- The time controling POST could have 2 versions. One that raises an error when the time is passed and another that cut the execution (using an external process) and raises an error. The second version could be very useful for debugging functions generating infinite loops 


GEN_SERVER_CPRE
===============

- Add to a gen_server_cpre a dictionary as an internal attribute that for each request stores a sorted list of waiting processes. This could ease the starvation checking.
- Add the previous state to the gen_server_cpre, and make the implementation so return an additional element (a flag) in the returned tuples indicaating whether the previous state should be updated. 
- solve starvation problems using invariants 
- Is it needed the old state in the invariant function
- The PRE/POST conditions in the gen_server maybe should make the client fail instead of the server.



Other
=====

- Generate edoc from contracts.
- Generate eunit tests from contracts.
- Generate property tests from contracts.
- Invariants when spawning a new process, such as its number of queued messages cannot be greater than 1, etc.
- Introduce locks
- pre in property testing to check that the inputs of a function have always some properties
```
	prop_calls_to_f() ->
	    ?FORALL({A,B}, {integer(),list(integer())},
		    ?PRE_CALLSTO(
		    	fun m:f/3,
		    	[m:g(A,B), m:h(A), m:i(B,A)],
		    	?P(1) > 3 andalso is_integer(?P(1)) andalso ?P(1) + ?P(2) > ?P(3)
		    )
		).
```
- liquid types
- liquid session types
- Add behaviour info as in https://github.com/uwiger/plain_fsm/blob/master/src/plain_fsm.erl 

To see
======

- "Starvation and Critical Race Analyzers for Ada"
- http://www.rise4fun.com/Dafny/
