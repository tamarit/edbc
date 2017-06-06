-module(edbc_free_vars_server). 

-export([init/0]).

-record(
	state,
	{
		variables = sets:new(), 
		max_length_variable = "", 
		current_id = 0
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
		exit ->
			ok;
		{add_variable, Variable} ->
			NewState = 
				State#state
				{
					variables = 
						sets:add_element(
							Variable, 
							State#state.variables)
				},
			loop(NewState);
		all_variables_added ->
			NewState = 
				State#state
				{
					variables = 
						sets:new(),
					max_length_variable = 
						sets:fold(
							fun get_max_length_variable/2,
							"",
							State#state.variables)
				},
			loop(NewState);
		{get_free_variable, Pid} ->
			CurrentId = 
				State#state.current_id,
			NewState =
				State#state{
					current_id = 
						CurrentId + 1
				},
			FreeVariable = 
					State#state.max_length_variable 
				++ 	integer_to_list(CurrentId), 
			Pid ! erl_syntax:variable(FreeVariable),
			loop(NewState);
		{get_free_id, Atom, Pid} ->
			CurrentId = 
				State#state.current_id,
			NewState =
				State#state{
					current_id = 
						CurrentId + 1
				},
			FreeID = 
					atom_to_list(Atom)
				++ 	integer_to_list(CurrentId), 
			Pid ! erl_syntax:atom(FreeID),
			loop(NewState);
		_Other ->
			loop(State)
	end.

get_max_length_variable(Variable, Max) 
		when length(Variable) > length(Max) ->
	Variable;
get_max_length_variable(_, Max) ->
	Max.
