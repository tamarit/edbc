-module(edbc_parse_transform).
-export([parse_transform/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_transform
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transform(Forms, _Options) ->
	% io:format("~p\n", [Forms]),
	unregister_servers(),
    register_servers(),
    FormsAnnBindings = 
		lists:map(
			fun annotate_bindings_form/1,
			Forms),
	% Send all variable names
	lists:map(
		fun send_vars/1,
		FormsAnnBindings),
	edbc_free_vars_server!all_variables_added,
	FormsPreTrans = 
		transform(FormsAnnBindings ,[], []),
	NewForms = 
		[erl_syntax:revert(IF) || IF <- FormsPreTrans],
	[io:format(erl_prettypr:format(F) ++ "\n") || F <- NewForms],
	NewForms.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tranform code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform([], _, Acc) -> 
	lists:reverse(Acc);
transform([Form | Forms], ToRemove, Acc) -> 
	case erl_syntax:type(Form) of 
		function -> 
			case 
				{
					erl_syntax:atom_value(erl_syntax:function_name(Form)), 
					erl_syntax:function_arity(Form)
				} 
			of 
				{pre, 0} -> 
					[NextFun | NewForms] = 
						Forms,
					{NewFunction, NextFunFresh, RemovedFun} = 
						transform_pre_function(
							NextFun, 
							extract_pre_post_fun(Form), 
							Acc ++ NewForms ++ ToRemove),
					transform(
						NewForms, 
						[RemovedFun | ToRemove], 
						[NextFunFresh, NewFunction | Acc] -- [RemovedFun]);
				{post, 0} -> 
					[OriginalFun, EntryFun | NewAcc] = 
						Acc,
					{NewEntryFun, RemovedFun} = 
						transform_post_function(
							EntryFun, 
							extract_pre_post_fun(Form), 
							Acc ++ Forms ++ ToRemove),
					transform(
						Forms, 
						[RemovedFun | ToRemove], 
						[OriginalFun, NewEntryFun | NewAcc] -- [RemovedFun]);
				_ -> 
					case lists:member(Form, ToRemove) of 
						true -> 
							transform(Forms, ToRemove, Acc);
						false -> 
							transform(Forms, ToRemove, [Form | Acc])
					end
			end;
		_ -> 
			transform(Forms, ToRemove, [Form | Acc])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRE transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_pre_function(Form, {NamePreFun, ArityPreFun}, OtherForms) ->
	FormName = 
		erl_syntax:function_name(Form),
	NewId = 
		get_free_id( 
			erl_syntax:atom_value(
				FormName)),
	NForm = 
		erl_syntax:function(
			NewId, 
			erl_syntax:function_clauses(Form)),
	FormPreFun = 
		search_fun({NamePreFun, ArityPreFun}, OtherForms),
	ParamOrderVars = 
		[ 	{Order, get_free_variable()} 
		|| 	Order <- lists:seq(1, erl_syntax:function_arity(NForm))],
	ParamVars = 
		element(2, lists:unzip(ParamOrderVars)),
	NewBodyFormPreFun = 
		replace_params(
			ParamOrderVars, 
			erl_syntax:clause_body(
				hd(erl_syntax:function_clauses(FormPreFun)))),
	ErrorExp = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(erlang),
				erl_syntax:atom(error)),
			[erl_syntax:string("The pre-condition is not hold")]),
	CallToFun = 
		erl_syntax:application(
			NewId,
			ParamVars),
	BodyInForm = 
		% hd(ParamVars),
		erl_syntax:case_expr(
			erl_syntax:block_expr(NewBodyFormPreFun),
			[erl_syntax:clause([erl_syntax:atom(true)], none, [CallToFun]),
			 erl_syntax:clause([erl_syntax:atom(false)], none, [ErrorExp])]),
	InForm = 
		erl_syntax:function(
			FormName,
			[erl_syntax:clause(
				ParamVars,
				none,
				[BodyInForm])]),
	{InForm, NForm, FormPreFun}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% POST transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_post_function(EntryForm, {NamePreFun, ArityPreFun}, OtherForms) ->
	FormPostFun = 
		search_fun({NamePreFun, ArityPreFun}, OtherForms),
	VarResult = 
		get_free_variable(),
	Parameters = 
		erl_syntax:clause_patterns(
			hd(erl_syntax:function_clauses(EntryForm))),
	ParamOrderVars = 
		lists:zip(lists:seq(1, length(Parameters)),Parameters),
	NewBodyFormPostFun = 
		replace_result(
			VarResult,
			replace_params(
				ParamOrderVars, 
				erl_syntax:clause_body(
					hd(erl_syntax:function_clauses(FormPostFun))))),
	CaseExpr = 
		hd(erl_syntax:clause_body(
				hd(erl_syntax:function_clauses(EntryForm)))),
	[TrueClause, FalseClause] = 
		erl_syntax:case_expr_clauses(CaseExpr),
	TrueClauseCall = 
		hd(erl_syntax:clause_body(TrueClause)),
	ErrorExp = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(erlang),
				erl_syntax:atom(error)),
			[erl_syntax:string("The post-condition is not hold")]),
	NewTrueClauseBody = 
		[
			erl_syntax:match_expr(VarResult, TrueClauseCall),
			erl_syntax:case_expr(
				erl_syntax:block_expr(NewBodyFormPostFun),
				[erl_syntax:clause([erl_syntax:atom(true)], none, [VarResult]),
				 erl_syntax:clause([erl_syntax:atom(false)], none, [ErrorExp])])		
		],
	NewBody = 
		erl_syntax:case_expr(
			erl_syntax:case_expr_argument(CaseExpr),
			[erl_syntax:clause([erl_syntax:atom(true)], none, NewTrueClauseBody),
			 FalseClause]),	
	NewEntryForm = 
		erl_syntax:function(
			erl_syntax:function_name(EntryForm),
			[erl_syntax:clause(
				Parameters,
				none,
				[NewBody])]),
	{NewEntryForm, FormPostFun}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_pre_post_fun(Form) -> 
	OpPreFun = 
		erl_syntax:implicit_fun_name(
			hd(erl_syntax:clause_body(
				hd(erl_syntax:function_clauses(Form))))),
	NamePreFun = 
		erl_syntax:atom_value(
			erl_syntax:arity_qualifier_body(OpPreFun)),
	ArityPreFun = 
		erl_syntax:integer_value(
			erl_syntax:arity_qualifier_argument(OpPreFun)),
	{NamePreFun, ArityPreFun}.

replace_params(DictParams, Es) -> 
	lists:map(
		fun(E) -> 
			erl_syntax_lib:map(
				fun(N) -> 
					case erl_syntax:type(N) of 
						application -> 
							Op = 
								erl_syntax:application_operator(N),
							case erl_syntax:type(Op) of 
								atom -> 
									case erl_syntax:atom_value(Op) of 
										p ->
											Param = 
												erl_syntax:integer_value(
													hd(erl_syntax:application_arguments(N))),
											hd([V || {P, V} <- DictParams, P == Param]);
										_ -> 
											N
									end;
								_ -> 
									N
							end;
						_ -> 
							N 
					end
				end,
				E)
		end,
		Es).
	
replace_result(VarRes, Es) -> 
	lists:map(
		fun(E) -> 
			erl_syntax_lib:map(
				fun(N) -> 
					case erl_syntax:type(N) of 
						application -> 
							Op = 
								erl_syntax:application_operator(N),
							case erl_syntax:type(Op) of 
								atom -> 
									case erl_syntax:atom_value(Op) of 
										r ->
											VarRes;
										_ -> 
											N
									end;
								_ -> 
									N
							end;
						_ -> 
							N 
					end
				end,
				E)
		end,
		Es).

search_fun({Name, Arity}, Forms) -> 
	try
		lists:foldl(
			fun
				(Form, not_found) ->
					case erl_syntax:type(Form) of 
						function -> 
							case 
								{
									erl_syntax:atom_value(
										erl_syntax:function_name(Form)), 
									erl_syntax:function_arity(Form)
								} 
							of 
								{Name, Arity} -> 
									throw({found, Form});
								_ -> 
									not_found
							end;
						_ -> 
							not_found
					end;
				% Not really needed because the throw expression when it is found
				(_, _) -> 
					not_found
			end,		
			not_found,
			Forms)
	catch 
		{found, Form} -> 
			Form
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotate Bindings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_bindings_form(Form)->
	annotate_bindings_form(
		erl_syntax:type(Form), 
		Form).

annotate_bindings_form(_, Form)->
	erl_syntax_lib:annotate_bindings(
		Form,
		ordsets:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variables sending
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_vars(Form) ->
	erl_syntax_lib:map(
		fun send_vars_node/1,
		Form).

send_vars_node(Node) ->
	case erl_syntax:type(Node) of 
		variable -> 
			edbc_free_vars_server!
				{
					add_variable, 
					erl_syntax:variable_literal(Node)
				};
		_ -> 
			ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unregister_servers() ->
	catch unregister(edbc_free_vars_server).

register_servers() ->
	register(
		edbc_free_vars_server, 
		spawn(edbc_free_vars_server, init, [])).

get_free_variable() ->
	edbc_free_vars_server ! {get_free_variable, self()},
	receive 
		Value ->
			Value
	end.

get_free_id(Atom) ->
	edbc_free_vars_server ! {get_free_id, Atom, self()},
	receive 
		Value ->
			Value
	end. 