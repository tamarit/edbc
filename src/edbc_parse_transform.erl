-module(edbc_parse_transform).
-export([parse_transform/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_transform
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transform(Forms, Options) ->
	% io:format("Options: ~p\n", [Options]),
	% case lists:member({d, edbc}, Options) of 
	% 	true -> 
	% 		io:format("EBDC ON\n");
	% 	false -> 
	% 		io:format("EBDC OFF\n")
	% end,
	% io:format("~p\n", [Forms]),
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
	% [io:format(erl_prettypr:format(F) ++ "\n") || F <- FormsAnnBindings],
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
	% [io:format(erl_prettypr:format(F) ++ "\n") || F <- NewForms],
	% [io:format("~p\n", [F]) || F <- NewForms, erl_syntax:type(F) == function, erl_syntax:atom_value(erl_syntax:function_name(F)) == post_invariant],
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
			NNotInvariantFuns ++ [build_post_invariant()]
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
														erl_syntax:atom(edbc_post_invariant),
														[
															FunInvariant, 
															erl_syntax:application(erl_syntax:atom(r), [])])])])])]),
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
% Free ariables managment
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Outputs of the functions modifying the state in gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% {ok,State}
% {ok,State,_}
% {noreply, NewState}
% {noreply, NewState,_}
% {stop, Reason, NewState}
% {reply, Reply, NewState}
% {reply, Reply, NewState,_}
% {reply, Reply, NewState,_}
% {stop, Reason, Reply, NewState}
% {true, State}
% {false, State}
% {error, Reason}

% fun() -> post_invariant(F) end

% post_invariant(F, R) -> 
% 	case R of 
% 		{ok, State} -> 
% 			F(State);
% 		{ok, State, _} -> 
% 			F(State);
% 		{noreply, State} -> 
% 			F(State);
% 		{noreply, State, _} -> 
% 			F(State);
% 		{stop, _, State} -> 
% 			F(State);
% 		{reply, _, State} -> 
% 			F(State);
% 		{reply, _, State, _} ->
% 			F(State);
% 		{stop, _, _, State} -> 
% 			F(State);
% 		{true, State} -> 
% 			F(State);
% 		{false, State} -> 
% 			F(State);
% 		{error, _} -> 
% 			true;
% 		ignore -> 
% 			true;
% 	end.

build_post_invariant() -> 
	{function,75,edbc_post_invariant,2,
	    [{clause,75,
	         [{var,75,'F'}, {var,75,'R'}],
	         [],
	         [{'case',76,
	              {var,76,'R'},
	              [{clause,77,
	                   [{tuple,77,[{atom,77,ok},{var,77,'State'}]}],
	                   [],
	                   [{call,78,{var,78,'F'},[{var,78,'State'}]}]},
	               {clause,79,
	                   [{tuple,79,[{atom,79,ok},{var,79,'State'},{var,79,'_'}]}],
	                   [],
	                   [{call,80,{var,80,'F'},[{var,80,'State'}]}]},
	               {clause,81,
	                   [{tuple,81,[{atom,81,noreply},{var,81,'State'}]}],
	                   [],
	                   [{call,82,{var,82,'F'},[{var,82,'State'}]}]},
	               {clause,83,
	                   [{tuple,83,
	                        [{atom,83,noreply},{var,83,'State'},{var,83,'_'}]}],
	                   [],
	                   [{call,84,{var,84,'F'},[{var,84,'State'}]}]},
	               {clause,85,
	                   [{tuple,85,[{atom,85,stop},{var,85,'_'},{var,85,'State'}]}],
	                   [],
	                   [{call,86,{var,86,'F'},[{var,86,'State'}]}]},
	               {clause,87,
	                   [{tuple,87,
	                        [{atom,87,reply},{var,87,'_'},{var,87,'State'}]}],
	                   [],
	                   [{call,88,{var,88,'F'},[{var,88,'State'}]}]},
	               {clause,89,
	                   [{tuple,89,
	                        [{atom,89,reply},
	                         {var,89,'_'},
	                         {var,89,'State'},
	                         {var,89,'_'}]}],
	                   [],
	                   [{call,90,{var,90,'F'},[{var,90,'State'}]}]},
	               {clause,91,
	                   [{tuple,91,
	                        [{atom,91,stop},
	                         {var,91,'_'},
	                         {var,91,'_'},
	                         {var,91,'State'}]}],
	                   [],
	                   [{call,92,{var,92,'F'},[{var,92,'State'}]}]},
	               {clause,91,
	                   [{tuple,91,
	                        [{atom,91,true},
	                         {var,91,'State'}]}],
	                   [],
	                   [{call,92,{var,92,'F'},[{var,92,'State'}]}]},
	               {clause,91,
	                   [{tuple,91,
	                        [{atom,91,false},
	                         {var,91,'State'}]}],
	                   [],
	                   [{call,92,{var,92,'F'},[{var,92,'State'}]}]},
	               {clause,93,
	                   [{tuple,93,[{atom,93,error},{var,93,'_'}]}],
	                   [],
	                   [{atom,94,true}]},
	                {clause,95,
	                   [{atom,95,ignore}],
	                   [],
	                   [{atom,96,true}]}]}]}]}.

