-module(edbc_parse_transform).
-export([parse_transform/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_transform
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transform(Forms, Options) ->
	EDBC_ON = 
		lists:member({d, edbc}, Options),
	unregister_servers(),
    register_servers(),
	FormsAnnBindings = 
		case EDBC_ON of 
			true -> 
			    FormsAnnBindings0 = 
					lists:map(
						fun annotate_bindings_form/1,
						Forms),
				% Send all variable names
				lists:map(
					fun send_vars/1,
					FormsAnnBindings0),
				edbc_free_vars_server!all_variables_added,
				FormsAnnBindings0;
			false -> 
				Forms
		end,
	FormsPreTrans0 =
		% FormsAnnBindings,
		replace_invariant_by_post(FormsAnnBindings),
	FormsPreTrans1 = 
		transform(FormsPreTrans0 ,[], [], EDBC_ON),
	% remove pre atoms between forms
	FormsPreTrans = 
		[F || F<- FormsPreTrans1, F /= pre],
	NewForms = 
		[erl_syntax:revert(IF) || IF <- FormsPreTrans],

	% Uncomment the line bellow to see the generated code
	% [io:format("~s\n", [lists:flatten(erl_prettypr:format(F))]) || F <- NewForms],

	% Uncomment the two lines bellow and modify FunName value to see the generated code for a concrete function
	% FunName = post_invariant,
	% [io:format("~p\n", [F]) || F <- NewForms, erl_syntax:type(F) == function, erl_syntax:atom_value(erl_syntax:function_name(F)) == FunName],

	NewForms.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tranform code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform([], _, Acc, _) -> 
	lists:reverse(Acc);
transform([Form | Forms], ToRemove, Acc, EDBC_ON) -> 
	case erl_syntax:type(Form) of 
		function -> 
			case 
				{
					erl_syntax:atom_value(erl_syntax:function_name(Form)), 
					erl_syntax:function_arity(Form)
				} 
			of 
				{edbc_pre, 0} -> 
					case EDBC_ON of 
						true -> 
							[NextFun | NewForms] = 
								Forms,
							{NewFunction, NextFunFresh, RemovedFuns} = 
								transform_pre_function(
									NextFun, 
									extract_pre_post_fun(Form), 
									Acc ++ NewForms ++ ToRemove),
							transform(
								NewForms, 
								RemovedFuns ++ ToRemove, 
								[NextFunFresh, pre, NewFunction | Acc] -- RemovedFuns,
								EDBC_ON);
						false -> 
							RemovedFuns = 
								[search_fun(
									extract_pre_post_fun(Form),  
									Acc ++ Forms ++ ToRemove)],
							transform(
								Forms, 
								RemovedFuns ++ ToRemove, 
								Acc -- RemovedFuns,
								EDBC_ON)
					end;
				{edbc_post, 0} ->
					case EDBC_ON of 
						true -> 
							{OriginalFun, EntryFun, NewAcc} = 
								case Acc of 
									[OriginalFun0, pre, EntryFun0 | NewAcc0] -> 
										{OriginalFun0, EntryFun0, NewAcc0};
									[OriginalFun0 | NewAcc0] ->
										{NewFunction, NextFunFresh, []} = 
											transform_pre_function(
												OriginalFun0, 
												[erl_syntax:atom(true)], 
												[]),
										{NextFunFresh, NewFunction, NewAcc0}
								end,
							{NewEntryFun, RemovedFuns} = 
								transform_post_function(
									EntryFun, 
									extract_pre_post_fun(Form), 
									Acc ++ Forms ++ ToRemove),
							transform(
								Forms, 
								RemovedFuns ++ ToRemove,  
								[OriginalFun, NewEntryFun | NewAcc] -- RemovedFuns,
								EDBC_ON);
						false -> 
							RemovedFuns = 
								[search_fun(
									extract_pre_post_fun(Form),  
									Acc ++ Forms ++ ToRemove)],
							transform(
								Forms, 
								RemovedFuns ++ ToRemove, 
								Acc -- RemovedFuns,
								EDBC_ON)
					end;
				{edbc_decreases, 0} ->
					case EDBC_ON of 
						true -> 
							[NextFun | NewForms] = 
								Forms,
							{NewFunction, NextFunFresh} = 
								transform_decreases_function(
									NextFun, 
									extract_decrease_paramenter(Form)),
							transform(
								NewForms, 
								ToRemove, 
								[NextFunFresh, NewFunction | Acc],
								EDBC_ON);
						false -> 
							transform(
								Forms, 
								ToRemove, 
								Acc,
								EDBC_ON)
					end;
				_ -> 
					case lists:member(Form, ToRemove) of 
						true -> 
							transform(Forms, ToRemove, Acc, EDBC_ON);
						false -> 
							transform(Forms, ToRemove, [Form | Acc], EDBC_ON)
					end
			end;
		_ -> 
			transform(Forms, ToRemove, [Form | Acc], EDBC_ON)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRE transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_pre_function(Form, FunOrBody, OtherForms) ->
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
	ParamOrderVars = 
		[ 	{Order, get_free_variable()} 
		|| 	Order <- lists:seq(1, erl_syntax:function_arity(NForm))],
	ParamVars = 
		element(2, lists:unzip(ParamOrderVars)),
	{FormPreFun, NewBodyFormPreFun} = 
		case FunOrBody of 
			{NamePreFun, ArityPreFun} -> 
				FormPreFun0 = 
					search_fun({NamePreFun, ArityPreFun}, OtherForms),
				NewBodyFormPreFun0 = 
					replace_params(
						ParamOrderVars, 
						erl_syntax:clause_body(
							hd(erl_syntax:function_clauses(FormPreFun0)))),
				{[FormPreFun0], NewBodyFormPreFun0};
			Body -> 
				NBody = 
					% Body,
					replace_params(
						ParamOrderVars, 
						Body),
				{[], NBody}
		end,
	ErrorExp = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(erlang),
				erl_syntax:atom(error)),
			[erl_syntax:string("The pre-condition is not hold.")]),
	ParErrorVar = 
		get_free_variable(),
	ParErrorExp = 
		fun(ErrorMsg) -> 
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:atom(erlang),
					erl_syntax:atom(error)),
				[erl_syntax:infix_expr(
					erl_syntax:string("The pre-condition is not hold: "), 
					erl_syntax:operator("++"),
					ErrorMsg)])
		end,
	CallToFun = 
		erl_syntax:application(
			NewId,
			ParamVars),
	BodyInForm = 
		% hd(ParamVars),
		erl_syntax:case_expr(
			erl_syntax:block_expr(NewBodyFormPreFun),
			[
				erl_syntax:clause([erl_syntax:atom(true)], none, [CallToFun]),
			 	erl_syntax:clause([erl_syntax:atom(false)], none, [ErrorExp]),
			 	erl_syntax:clause(
				 	[erl_syntax:tuple(
				 		[erl_syntax:atom(false), ParErrorVar])], 
				 	none, 
				 	[ParErrorExp(ParErrorVar)])
			]),
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

transform_post_function(EntryForm, FunOrBody, OtherForms) ->
	VarResult = 
		get_free_variable(),
	Parameters = 
		erl_syntax:clause_patterns(
			hd(erl_syntax:function_clauses(EntryForm))),
	ParamOrderVars = 
		lists:zip(lists:seq(1, length(Parameters)),Parameters),
	{FormPostFun, NewBodyFormPostFun} = 
		case FunOrBody of 
			{NamePreFun, ArityPreFun} -> 
				% io:format("~p\n", [{NamePreFun, ArityPreFun}]),
				FormPostFun0 = 
					search_fun({NamePreFun, ArityPreFun}, OtherForms),
				% io:format("~p\n", [FormPostFun0]),
				NewBodyFormPostFun0 = 
					replace_result(
						VarResult,
						replace_params(
							ParamOrderVars, 
							erl_syntax:clause_body(
								hd(erl_syntax:function_clauses(FormPostFun0))))),
				{[FormPostFun0], NewBodyFormPostFun0};
			Body -> 
				% io:format("Body: " ++ erl_prettypr:format(Body) ++ "\n"),
				NBody = 
					replace_result(
						VarResult,
						replace_params(
							ParamOrderVars, 
							Body)),
				% io:format("NBody: " ++ erl_prettypr:format(NBody) ++ "\n"),
				{[], NBody}
		end,
	% FormPostFun = 
	% 	search_fun({NamePreFun, ArityPreFun}, OtherForms),
	% NewBodyFormPostFun = 
	% 	replace_result(
	% 		VarResult,
	% 		replace_params(
	% 			ParamOrderVars, 
	% 			erl_syntax:clause_body(
	% 				hd(erl_syntax:function_clauses(FormPostFun))))),
	CaseExpr = 
		hd(erl_syntax:clause_body(
				hd(erl_syntax:function_clauses(EntryForm)))),
	[TrueClause, FalseClause, ParFalseClause] = 
		erl_syntax:case_expr_clauses(CaseExpr),
	TrueClauseCall = 
		hd(erl_syntax:clause_body(TrueClause)),
	ErrorExp = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(erlang),
				erl_syntax:atom(error)),
			[erl_syntax:string("The post-condition is not hold.")]),
	ParErrorVar = 
		get_free_variable(),
	ParErrorExp = 
		fun(ErrorMsg) -> 
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:atom(erlang),
					erl_syntax:atom(error)),
				[erl_syntax:infix_expr(
					erl_syntax:string("The post-condition is not hold: "), 
					erl_syntax:operator("++"),
					ErrorMsg)])
		end,
	NewTrueClauseBody = 
		[
			erl_syntax:match_expr(VarResult, TrueClauseCall),
			erl_syntax:case_expr(
				erl_syntax:block_expr(NewBodyFormPostFun),
				[
					erl_syntax:clause([erl_syntax:atom(true)], none, [VarResult]),
				 	erl_syntax:clause([erl_syntax:atom(false)], none, [ErrorExp]),
				 	erl_syntax:clause(
					 	[erl_syntax:tuple(
					 		[erl_syntax:atom(false), ParErrorVar])], 
					 	none, 
					 	[ParErrorExp(ParErrorVar)])
				 ])		
		],
	NewBody = 
		erl_syntax:case_expr(
			erl_syntax:case_expr_argument(CaseExpr),
			[erl_syntax:clause([erl_syntax:atom(true)], none, NewTrueClauseBody),
			 FalseClause,
			 ParFalseClause]),	
	NewEntryForm = 
		erl_syntax:function(
			erl_syntax:function_name(EntryForm),
			[erl_syntax:clause(
				Parameters,
				none,
				[NewBody])]),
	{NewEntryForm, FormPostFun}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % INVARIANT transformation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace_invariant_by_post(Forms) ->
	InvariantFuns =  
		[ 
			Form
		|| 
			Form <- Forms, 
			erl_syntax:type(Form) == function, 
			erl_syntax:atom_value(erl_syntax:function_name(Form)) == edbc_invariant,
			erl_syntax:function_arity(Form) == 0
		],
	case InvariantFuns of 
		[] -> 
			% io:format("No invariants\n"),
			Forms;
		[IF|_] -> 
			NotInvariantFuns = 
				Forms -- InvariantFuns,
			{FunInvariantName, FunInvariantArity} = 
				extract_pre_post_fun(IF),
			FunInvariant = 
				erl_syntax:implicit_fun(
					% erl_syntax:arity_qualifier(
						erl_syntax:atom(FunInvariantName), 
						erl_syntax:integer(FunInvariantArity)),
				% ),
			% io:format("PF: " ++ erl_prettypr:format(FunInvariant) ++ "\n"),
			NNotInvariantFuns = 
				add_post_after(
					NotInvariantFuns,
					FunInvariant,
					[
						{code_change, 3},
						{handle_call, 3},
						{handle_cast, 2},
						{handle_info, 2},
						{init, 1},
						{cpre, 3}
					]),
			% NNotInvariantFuns ++ [erl_syntax_lib:map(fun(X) -> X end, build_post_invariant())]
			% NNotInvariantFuns ++ [build_post_invariant()]
			NNotInvariantFuns
	end.

add_post_after(Forms, _, []) -> 
	Forms;
add_post_after(Forms, FunInvariant, [FunGenServer | FunsGenServer]) -> 
	NFormsRev = 
		lists:foldl(
			fun (Form, Acc)   ->
				case erl_syntax:type(Form) of 
					function -> 
						case 
							{
								erl_syntax:atom_value(erl_syntax:function_name(Form)), 
								erl_syntax:function_arity(Form)
							} 
						of 
							FunGenServer ->
								FunPost = 
									erl_syntax:function(
										erl_syntax:atom(edbc_post), 
										[erl_syntax:clause(
											[],
											none,
											[erl_syntax:fun_expr([
												erl_syntax:clause(
													[],
													none,
													[erl_syntax:application(
														% erl_syntax:atom(edbc_post_invariant),
														erl_syntax:module_qualifier(
															erl_syntax:atom(edbc_lib),
															erl_syntax:atom(post_invariant)),
														[
															FunInvariant, 
															erl_syntax:application(erl_syntax:atom(edbc_r), [])])])])])]),
								% io:format("FunPost:~p\n", [FunPost]),
									% edbc_post() -> fun() -> post_invariant(F) end 
								[FunPost, Form | Acc];
							_ -> 
								[Form | Acc]
						end;
					_ -> 
						[Form | Acc]
				end 
			end,
			[],
			Forms),
	add_post_after(lists:reverse(NFormsRev), FunInvariant, FunsGenServer).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECREASES transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_decreases_function(Function, ParNumber) ->
	% io:format("ParNumber: ~p\n", [ParNumber]),
	FunctionName =
		erl_syntax:function_name(Function),
	AuxFunctionName = 
		get_free_id(
			erl_syntax:atom_value(
				FunctionName)),
	FunctionClauses = 
		erl_syntax:function_clauses(Function),
	NewFunctionClauses = 
		lists:map(
			fun(Clause) -> 
				replace_recursive_calls(
					Clause, 
					FunctionName, 
					AuxFunctionName, 
					ParNumber)
			end,
			FunctionClauses),
	NewFunction = 
		erl_syntax:function(
			FunctionName,
			NewFunctionClauses),
	AuxFunctionOldValuePar = 
		get_free_variable(),
	AuxFunctionPars0 =
		[get_free_variable() || _ <- lists:seq(1, length(erl_syntax:clause_patterns(hd(FunctionClauses))))],
	AuxFunctionNewValuePar =
		lists:nth(
			ParNumber,
			AuxFunctionPars0),
	AuxFunctionPars = 
		[AuxFunctionOldValuePar | AuxFunctionPars0],
	AuxFunctionClauseCall = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(edbc_lib),
				erl_syntax:atom(decreasing_check)),
			[
				AuxFunctionNewValuePar,
				AuxFunctionOldValuePar,
				erl_syntax:fun_expr([
					erl_syntax:clause(
							[],
							none,
							[erl_syntax:application(
								FunctionName, 
								AuxFunctionPars0)]
						)])
			]),
	AuxFunctionClause = 
		erl_syntax:clause(
			AuxFunctionPars,
			none,
			[AuxFunctionClauseCall]),
	AuxFunction = 
		erl_syntax:function(
			AuxFunctionName,
			[AuxFunctionClause]),
	{NewFunction, AuxFunction}.
	
replace_recursive_calls(Clause, FunctionName, AuxFunctionName, ParNumber) -> 
	Parameter = 
		lists:nth(
			ParNumber,
			erl_syntax:clause_patterns(Clause)),
	erl_syntax_lib:map(
		fun(Node) -> 
			case erl_syntax:type(Node) of 
				application -> 
					Op = erl_syntax:application_operator(Node),
					case erl_syntax:type(Op) of 
						atom -> 
							case erl_syntax:atom_value(FunctionName) == erl_syntax:atom_value(Op) of 
								true -> 
									erl_syntax:application(
										AuxFunctionName, 
										[Parameter | erl_syntax:application_arguments(Node)]);
								false -> 
									Node
							end;
						_ -> 
							Node
					end;
				_ -> 
					Node
			end
		end,
		Clause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_pre_post_fun(Form) -> 
	BodyForm = 
		hd(erl_syntax:clause_body(
			hd(erl_syntax:function_clauses(Form)))),
	case erl_syntax:type(BodyForm) of 
		fun_expr ->  
			BodyFun = 
				erl_syntax:clause_body(
					hd(erl_syntax:fun_expr_clauses(BodyForm))),
			BodyFun;
		implicit_fun -> 
			OpPreFun = 
				erl_syntax:implicit_fun_name(BodyForm),
			NamePreFun = 
				erl_syntax:atom_value(
					erl_syntax:arity_qualifier_body(OpPreFun)),
			ArityPreFun = 
				erl_syntax:integer_value(
					erl_syntax:arity_qualifier_argument(OpPreFun)),
			{NamePreFun, ArityPreFun}
	end.


extract_decrease_paramenter(Form) -> 
	ErrorMsg = 
		fun() -> 
			error("There should be a parameter, i.e. ?P(N), in the decrease contract.")
		end,
	BodyForm = 
		hd(erl_syntax:clause_body(
			hd(erl_syntax:function_clauses(Form)))),
	case erl_syntax:type(BodyForm) of 
		application -> 
			Op = 
				erl_syntax:application_operator(BodyForm),
			case erl_syntax:type(Op) of 
				atom -> 
					case erl_syntax:atom_value(Op) of 
						edbc_p ->
							case length(erl_syntax:application_arguments(BodyForm)) of 
								1 -> 
									Param = 
										erl_syntax:integer_value(
											hd(erl_syntax:application_arguments(BodyForm))),
									Param;
								_ -> 
									ErrorMsg()
							end;
						_ -> 
							ErrorMsg()
					end;
				_ -> 
					ErrorMsg()
			end;
		_ -> 
			ErrorMsg() 
	end.

replace_params(DictParams, Es) -> 
	% io:format("DictParams: ~p\nEs: ~p\n", [DictParams, Es]),
	lists:map(
		fun(E) -> 
			% io:format("ARRIBA1\n"),
			erl_syntax_lib:map(
				fun(N) -> 	
					% io:format("ARRIBA2\n"),
					case erl_syntax:type(N) of 
						application -> 
							% io:format("ARRIBA3\n"),
							Op = 
								erl_syntax:application_operator(N),
							case erl_syntax:type(Op) of 
								atom -> 
									case erl_syntax:atom_value(Op) of 
										edbc_p ->
											case length(erl_syntax:application_arguments(N)) of 
												1 -> 
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
										edbc_r ->
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
				(Form, not_found) when Form /= pre ->
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
				% Not really needed because there is a throw expression when it is found
				(_, _) -> 
					not_found
			end,		
			not_found,
			Forms)
	catch 
		{found, Form} -> 
			Form
	end;
search_fun(Other, Forms) ->
	io:format("Other: ~p\n", [Other]),
	not_found.

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
% Free variables managment
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

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % PRE_I transformation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% transform_internal_pre_function(Form, {NamePreFun, ArityPreFun}, OtherForms) ->
% 	FormName = 
% 		erl_syntax:function_name(Form),
% 	NewId = 
% 		get_free_id( 
% 			erl_syntax:atom_value(
% 				FormName)),
% 	NForm = 
% 		erl_syntax:function(
% 			NewId, 
% 			erl_syntax:function_clauses(Form)),
% 	FormPreFun = 
% 		search_fun({NamePreFun, ArityPreFun}, OtherForms),
% 	ParamOrderVars = 
% 		[ 	{Order, get_free_variable()} 
% 		|| 	Order <- lists:seq(1, erl_syntax:function_arity(NForm))],
% 	ParamVars = 
% 		element(2, lists:unzip(ParamOrderVars)),
% 	NewBodyFormPreFun = 
% 		replace_params(
% 			ParamOrderVars, 
% 			erl_syntax:clause_body(
% 				hd(erl_syntax:function_clauses(FormPreFun)))),
% 	ErrorExp = 
% 		erl_syntax:application(
% 			erl_syntax:module_qualifier(
% 				erl_syntax:atom(erlang),
% 				erl_syntax:atom(error)),
% 			[erl_syntax:string("The pre-condition is not hold")]),
% 	CallToFun = 
% 		erl_syntax:application(
% 			NewId,
% 			ParamVars),
% 	BodyInForm = 
% 		% hd(ParamVars),
% 		erl_syntax:case_expr(
% 			erl_syntax:block_expr(NewBodyFormPreFun),
% 			[erl_syntax:clause([erl_syntax:atom(true)], none, [CallToFun]),
% 			 erl_syntax:clause([erl_syntax:atom(false)], none, [ErrorExp])]),
% 	InForm = 
% 		erl_syntax:function(
% 			FormName,
% 			[erl_syntax:clause(
% 				ParamVars,
% 				none,
% 				[BodyInForm])]),
% 	{InForm, NForm, FormPreFun}.

