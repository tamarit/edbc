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
				% Bindings not used
			  %   FormsAnnBindings0 = 
					% lists:map(
					% 	fun annotate_bindings_form/1,
					% 	Forms),
				FormsAnnBindings0 = 
					Forms,
				% Send all variable names
				lists:map(
					fun send_vars/1,
					FormsAnnBindings0),
				edbc_free_vars_server!all_variables_added,
				FormsAnnBindings0;
			false -> 
				Forms
		end,
	Form1 = 
		search_ebdc_funs(FormsAnnBindings),
	Form2 = 
		build_funs(Form1, EDBC_ON),

	% uncomment to print the pretty-printted version of the code
	% [io:format("~s\n", [lists:flatten(erl_prettypr:format(F))]) || F <- Form2],

	NewForms = 
		[erl_syntax:revert(IF) || IF <- Form2],
	NewForms.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotate contracts 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search_ebdc_funs(Forms) ->
	FunGetNewAccForms = 
		fun
			(none, AccForms) -> 
				AccForms;
			(PrevFun, AccForms) -> 
				[PrevFun | AccForms]
		end,
	{FForms, _, FPrevFun, FInvariants} = 
		lists:foldl( 
			fun(Form, {AccForms, AccPres, PrevFun, AccInvariants}) -> 
				case erl_syntax:type(Form) of 
					function -> 
						case fun_name_arity(Form) of 
							{edbc_pre, 0} -> 
								{
									FunGetNewAccForms(PrevFun, AccForms), 
									[Form | AccPres], 
									none, 
									AccInvariants
								};
							{edbc_decreases, 0} -> 
								{
									FunGetNewAccForms(PrevFun, AccForms),  
									[Form | AccPres], 
									none, 
									AccInvariants
								};
							{edbc_expected_time, 0} -> 
								{
									FunGetNewAccForms(PrevFun, AccForms),  
									[Form | AccPres], 
									none, 
									AccInvariants
								};
							{edbc_timeout, 0} -> 
								{
									FunGetNewAccForms(PrevFun, AccForms),  
									[Form | AccPres], 
									none, 
									AccInvariants
								};
							{edbc_pure, 0} -> 
								{
									FunGetNewAccForms(PrevFun, AccForms), 
									[Form | AccPres], 
									none, 
									AccInvariants
								};
							{edbc_post, 0} ->
								NPrevFun =
									case PrevFun of 
										none -> 
											PrevFun;
										_ -> 
											case erl_syntax:get_ann(PrevFun) of
												[{PREs, POSTs}] -> 
													erl_syntax:set_ann(
														PrevFun,
														[{PREs, POSTs ++ [Form]}]);
												_ -> 
													PrevFun
											end
									end,
								{
									AccForms, 
									[], 
									NPrevFun, 
									AccInvariants
								};
							{edbc_invariant, 0} -> 
								{
									AccForms, 
									AccPres, 
									PrevFun, 
									[Form | AccInvariants]
								};	
							_ -> 
								NPrevFun = 
									erl_syntax:set_ann(
										Form,
										[{lists:reverse(AccPres), []}]),
								{
									FunGetNewAccForms(PrevFun, AccForms),  
									[], 
									NPrevFun, 
									AccInvariants
								}
						end;
					_ ->
						{[Form | AccForms], AccPres, PrevFun, AccInvariants}
				end
			end,
			{[], [], none, []},
			Forms),
	NFForms = 
		FunGetNewAccForms(FPrevFun, FForms),
	FunsChangeState = 
		[
			{code_change, 3},
			{handle_call, 3},
			{handle_cast, 2},
			{handle_info, 2},
			{init, 1},
			{cpre, 3}
		],
	NFInvariants = 
		[begin
			{FunInvariantName, FunInvariantArity} = 
				extract_pre_post_fun(FI),
			FunInvariant = 
				erl_syntax:implicit_fun(
						erl_syntax:atom(FunInvariantName), 
						erl_syntax:integer(FunInvariantArity)),
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
								erl_syntax:module_qualifier(
									erl_syntax:atom(edbc_lib),
									erl_syntax:atom(post_invariant)),
								[
									FunInvariant, 
									erl_syntax:application(erl_syntax:atom(edbc_r), [])])])])])])
		end
		|| FI <- FInvariants],
	lists:map( 
		fun(Form) -> 
			case erl_syntax:type(Form) of
				function -> 
					case lists:member(fun_name_arity(Form), FunsChangeState) of 
						true ->  
							case erl_syntax:get_ann(Form) of
								[{PREs, POSTs}] -> 
									erl_syntax:set_ann(
										Form,
										[{PREs, POSTs ++ NFInvariants}]);
								_ -> 
									Form
							end;
						false -> 
							Form
					end;
				_ ->
					Form
			end
		end,
		lists:reverse(NFForms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tranform contracts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_funs(Forms, EDBC_ON) -> 
	{NForms0, ToRemove0} = 
		lists:foldl(
			fun(Form0, {Acc, ToRemove}) -> 
				case erl_syntax:type(Form0) of 
					function ->
						% All functions have an annotation
						[{PREs, POSTs}] = 
							erl_syntax:get_ann(Form0),
						Form = 
							erl_syntax:set_ann(Form0, []),
						{PreDec0, PreOther0} = 
							lists:foldl(
								fun(FormPre, {AccDec, AccOther}) ->
									case fun_name_arity(FormPre) of
										{edbc_decreases, 0} -> 
											{[FormPre | AccDec], AccOther};
										_Other ->
											% io:format("Other: ~p\n", [_Other]),
											{AccDec, [FormPre | AccOther]}
									end
								end,
								{[], []},
								PREs),
						{PreDec, PreOther} =
							{lists:reverse(PreDec0), lists:reverse(PreOther0)},
						{NewForm, NAcc} = 
							case {PreDec, EDBC_ON} of 
								{[PreDecFun | _], true} ->	
									{NewForm0, AuxFun} = 	
										transform_decreases_function(
											Form,
											extract_decrease_paramenter(PreDecFun)),
									{
										NewForm0,
										[AuxFun | Acc]
									};
								_ ->
									{
										Form, 
										Acc
									}
							end,
						{FForm, NewFuns, NToRemove} = 
							lists:foldl(
								fun(ContractFun, {CurrentForm, NewFuns, ToRemove}) -> 
									case {fun_name_arity(ContractFun),EDBC_ON} of 
										{{edbc_pre, 0}, true} -> 
											{NewFunction, NCurrentForm, RemovedFuns} = 
												transform_pre_function(
													CurrentForm, 
													extract_pre_post_fun(ContractFun), 
													Forms),
											{
												NCurrentForm, 
												[NewFunction | NewFuns], 
												ToRemove ++ RemovedFuns
											};
										{{edbc_expected_time, 0}, true} -> 
											{NewFunction, NCurrentForm, RemovedFuns} = 
												transform_expected_time_function(
													CurrentForm, 
													extract_pre_post_fun(ContractFun), 
													Forms),
											{
												NCurrentForm, 
												[NewFunction | NewFuns], 
												ToRemove ++ RemovedFuns
											};
										{{edbc_timeout, 0}, true} -> 
											{NewFunction, NCurrentForm, RemovedFuns} = 
												transform_timeout_function(
													CurrentForm, 
													extract_pre_post_fun(ContractFun), 
													Forms),
											{
												NCurrentForm, 
												[NewFunction | NewFuns], 
												ToRemove ++ RemovedFuns
											};
										{{edbc_pure, 0}, true} -> 
											{NewFunction, NCurrentForm, RemovedFuns} = 
												transform_pure_function(
													CurrentForm, 
													[erl_syntax:atom(true)], 
													Forms),
											{
												NCurrentForm, 
												[NewFunction | NewFuns], 
												ToRemove ++ RemovedFuns
											};
										{{edbc_post, 0}, true} -> 
											{NewFunction, NCurrentForm, RemovedFuns} = 
												transform_post_function(
													CurrentForm, 
													extract_pre_post_fun(ContractFun), 
													Forms),
											{
												NCurrentForm, 
												[NewFunction | NewFuns], 
												ToRemove ++ RemovedFuns
											};
										_ -> 
											RemovedFuns = 
												[search_fun(
													extract_pre_post_fun(ContractFun),  
													Forms)],
											{
												CurrentForm,
												NewFuns,
												ToRemove ++ RemovedFuns
											}
									end
								end,
								{NewForm, [], []},
								PreOther ++ POSTs),
						{[FForm | NewFuns ++ NAcc], ToRemove ++ NToRemove};
					_ -> 
						{[Form0 | Acc], ToRemove}
				end
			end,
			{[], []},
			Forms),
	NForms1 = 
		remove_funs(NForms0, ToRemove0),
	NForms1.

remove_funs(Forms, ToRemove) -> 
	ToRemoveFunArity = 
		lists:map(
			fun fun_name_arity/1,
			ToRemove),
	lists:foldl(
		fun(Form, Acc) -> 
			case erl_syntax:type(Form) of 
				function -> 
					case lists:member(fun_name_arity(Form), ToRemoveFunArity) of 
						true -> 
							Acc;
						false -> 
							[Form | Acc]
					end;
				_ -> 
					[Form | Acc]
			end
		end,
		[],
		Forms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRE/POST transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_pre_post_function(Form, FunOrBody, OtherForms, IsPost, LibFunction) ->
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
	VarResult = 
		get_free_variable(),
	{FormPostFun, NewBodyFormPostFun} = 
		case FunOrBody of 
			{NamePreFun, ArityPreFun} -> 
				% io:format("~p\n", [{NamePreFun, ArityPreFun}]),
				FormPostFun0 = 
					search_fun({NamePreFun, ArityPreFun}, OtherForms),
				% io:format("~p\n", [FormPostFun0]),
				NewBodyFormPostFun0 = 
					case IsPost of 
						true -> 
							replace_result(
								VarResult,
								replace_params(
									ParamOrderVars, 
									erl_syntax:clause_body(
										hd(erl_syntax:function_clauses(FormPostFun0)))));
						false -> 
							replace_params(
								ParamOrderVars, 
								erl_syntax:clause_body(
									hd(erl_syntax:function_clauses(FormPostFun0))))
					end,
				{[FormPostFun0], NewBodyFormPostFun0};
			Body -> 
				NBody = 
					case IsPost of
						true -> 
							replace_result(
								VarResult,
								replace_params(
									ParamOrderVars, 
									Body));
						false -> 
							replace_params(
								ParamOrderVars, 
								Body)
					end,
				{[], NBody}
		end,
	CallToFun = 
		erl_syntax:application(
			NewId,
			ParamVars),
	BodyInForm = 
		erl_syntax:application(
			erl_syntax:module_qualifier(
				erl_syntax:atom(edbc_lib),
				erl_syntax:atom(LibFunction)),
				[
					erl_syntax:fun_expr([
						erl_syntax:clause(
								[VarResult || IsPost],
								none,
								NewBodyFormPostFun
							)]),
					erl_syntax:fun_expr([
						erl_syntax:clause(
								[],
								none,
								[CallToFun]
							)])
				]),
	InForm = 
		erl_syntax:function(
			FormName,
			[erl_syntax:clause(
				ParamVars,
				none,
				[BodyInForm])]),
	{InForm, NForm, FormPostFun}.

transform_pre_function(Form, FunOrBody, OtherForms) ->
	transform_pre_post_function(Form, FunOrBody, OtherForms, false, pre).

transform_expected_time_function(Form, FunOrBody, OtherForms) ->
	transform_pre_post_function(Form, FunOrBody, OtherForms, false, expected_time).

transform_timeout_function(Form, FunOrBody, OtherForms) ->
	transform_pre_post_function(Form, FunOrBody, OtherForms, false, timeout).

transform_pure_function(Form, FunOrBody, OtherForms) ->
	transform_pre_post_function(Form, FunOrBody, OtherForms, false, is_pure).

transform_post_function(Form, FunOrBody, OtherForms) ->
	transform_pre_post_function(Form, FunOrBody, OtherForms, true, post).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECREASES transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_decreases_function(Function, ParNumbers) ->
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
					ParNumbers)
			end,
			FunctionClauses),
	NewFunction = 
		erl_syntax:function(
			FunctionName,
			NewFunctionClauses),
	AuxFunctionOldValuePar = 
		get_free_variable(),
	AuxFunctionPars0 =
		[get_free_variable() 
		|| _ <- lists:seq(1, erl_syntax:function_arity(Function))],
	AuxFunctionNewValuePar =
		erl_syntax:list(
			[lists:nth(
				ParNumber,
				AuxFunctionPars0)
			|| ParNumber <- ParNumbers]),
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
	

replace_recursive_calls(Clause, FunctionName, AuxFunctionName, ParNumbers) -> 
	Parameters = 
		erl_syntax:list(
			[lists:nth(
				ParNumber,
				erl_syntax:clause_patterns(Clause))
			|| ParNumber <- ParNumbers]),
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
										[Parameters | erl_syntax:application_arguments(Node)]);
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
	FunExtractPar = 
		fun(App) -> 
			Op = 
				erl_syntax:application_operator(App),
			edbc_p = 
				erl_syntax:atom_value(Op),
			1 = 
				length(erl_syntax:application_arguments(App)),
			Arg = 
				hd(erl_syntax:application_arguments(App)),
			erl_syntax:integer_value(Arg)
		end,
	try
		BodyForm = 
			hd(erl_syntax:clause_body(
				hd(erl_syntax:function_clauses(Form)))),
		case erl_syntax:type(BodyForm) of 
			application -> 
				[FunExtractPar(BodyForm)];
			list ->
				[FunExtractPar(E) || E <- erl_syntax:list_elements(BodyForm)]
		end
	catch
		_:_ -> 
			error("There should be a parameter or a list of parameters, i.e. ?P(N) or [?P(M),?P(N)], in the decrease contract.")
	end.

replace_params(DictParams, Es) -> 
	lists:map(
		fun(E) -> 
			erl_syntax_lib:map(
				fun(N) -> 	
					try 
						Op = erl_syntax:application_operator(N),
						edbc_p = erl_syntax:atom_value(Op),
						1 = length(erl_syntax:application_arguments(N)),
						Param = 
							erl_syntax:integer_value(
								hd(erl_syntax:application_arguments(N))),
						hd([V || {P, V} <- DictParams, P == Param])
					catch 
						_:_ -> 
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
					try 
						Op = erl_syntax:application_operator(N),
						edbc_r = erl_syntax:atom_value(Op),
						0 = length(erl_syntax:application_arguments(N)),
						VarRes
					catch 
						_:_ -> 
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
							case fun_name_arity(Form) of 
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
	% io:format("Other: ~p\n", [Other]),
	not_found.


fun_name_arity(Form) -> 
	{
		erl_syntax:atom_value(
			erl_syntax:function_name(Form)), 
		erl_syntax:function_arity(Form)
	}.

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
