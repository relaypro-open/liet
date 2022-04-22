-module(liet_transform).

-export([parse_transform/2, compile/1]).

parse_transform(AST, _Options) ->
    walk_ast([], AST).

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    walk_ast([{function, Line, Name, Arity,
               walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H)|Acc], T).

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, liet},
                                  {atom, _Line3, compile}}, [Arg]}) ->
    % liet:compile
    % Transforms list of tuples that meet the liet:compile code contract into
    % the dependency graph map required for liet:start
    compile(Arg, Line);
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

compile(AST) ->
    compile(AST, 1).

compile(AST, Line) ->
    {tuple, Line, [{atom, Line, ok}, {map, Line, do_liet_compile([], AST)}]}.

do_liet_compile(MapFieldAssocAcc, {nil, _Line}) ->
    lists:reverse(MapFieldAssocAcc);
do_liet_compile(MapFieldAssocAcc, {cons, Line,
                                   {tuple, _Line0,
                                   [{atom, _Line1, Name}|LietCode]}, ConsTail}) ->
    {ApplyAST, DestroyAST} = case LietCode of
                                [ApplyCode, DestroyCode] -> {ApplyCode, DestroyCode};
                                [ApplyCode] -> {ApplyCode, {atom, Line, ok}}
                            end,
    NormalizedApplyAST = normalize_for_provider(ApplyAST),
    NormalizedDestroyAST = normalize_for_provider(DestroyAST),
    ApplyDeps = find_deps(NormalizedApplyAST),
    DestroyDeps = find_deps(NormalizedDestroyAST),
    Deps = unique_deps(ApplyDeps ++ DestroyDeps, #{}),
    TaskMap = {map, Line,
               [
                {map_field_assoc, Line, {atom, Line, deps}, list_to_cons(Deps, Line)},
                {map_field_assoc, Line, {atom, Line, apply}, NormalizedApplyAST},
                {map_field_assoc, Line, {atom, Line, destroy}, NormalizedDestroyAST},
                {map_field_assoc, Line, {atom, Line, args}, {atom, Line, undefined}}
               ]},
    NewMapField = {map_field_assoc, Line,
                   {atom, Line, Name},
                   TaskMap
                   },
    do_liet_compile([NewMapField|MapFieldAssocAcc], ConsTail);
do_liet_compile(_MapFieldAssocAcc, Arg) ->
    io:format("unmatched arg=~p~n", [Arg]),
    erlang:error(badarg).

normalize_for_provider({'fun', Line, {clauses, Clauses}}) ->
    {'fun', Line, {clauses, normalize_clauses_for_provider([], Clauses)}};
normalize_for_provider(AST) ->
    %% turns a simple expression into an arity-2 anonymous function
    Line = element(2, AST),
    Clauses = [{clause, Line,
        [{var, Line, '_'},       % _Arg
         {var, Line, '_'}],      % _State
        [],                      % guards
        [AST]}],
    Clauses2 = normalize_clauses_for_provider([], Clauses),
    {'fun', Line,
     {clauses, Clauses2}}.

normalize_clauses_for_provider(Acc, []) ->
    lists:reverse(Acc);
normalize_clauses_for_provider(Acc, [{clause, Line, [Var1AST,
                                                     {var, Line2, VarName2}], Guards, Expressions}|T]) ->
    LietRefs = find_liet_refs_in_expressions([], Expressions),
    StateVarName = select_state_var_name(LietRefs, atom_to_list(VarName2)),
    Expressions2 = normalize_expressions_for_provider(StateVarName, Expressions),
    Clause2 = {clause, Line, [Var1AST, {var, Line2, StateVarName}], Guards, Expressions2},
    normalize_clauses_for_provider([Clause2|Acc], T);
normalize_clauses_for_provider(Acc, [{clause, Line, [Var1AST], Guards, Expressions}|T]) ->
    %% assume single var is the State, normalize to arity-2
    LietRefs = find_liet_refs_in_expressions([], Expressions),
    StateVarName = select_state_var_name(LietRefs, "_"),
    Expressions2 = normalize_expressions_for_provider(StateVarName, Expressions),
    Clause2 = {clause, Line, [Var1AST, {var, Line, StateVarName}], Guards, Expressions2},
    normalize_clauses_for_provider([Clause2|Acc], T);
normalize_clauses_for_provider(Acc, [{clause, Line, [], Guards, Expressions}|T]) ->
    %% no vars
    LietRefs = find_liet_refs_in_expressions([], Expressions),
    StateVarName = select_state_var_name(LietRefs, "_"),
    Expressions2 = normalize_expressions_for_provider(StateVarName, Expressions),
    Clause2 = {clause, Line, [{var, Line, '_'}, {var, Line, StateVarName}], Guards, Expressions2},
    normalize_clauses_for_provider([Clause2|Acc], T).

select_state_var_name(List, "_" ++ _) when length(List) > 0 -> 'StateVar@@';
select_state_var_name(_, X) -> list_to_atom(X).

normalize_expressions_for_provider(StateVar, {call, Line, {remote, Line1, {atom, Line2, liet},
                                                            {atom, Line3, Name}}, []}) ->
    {call, Line,
     {remote, Line1, {atom, Line2, liet},
      {atom, Line3, get}},
      [{atom, Line3, Name},
       {var, Line3, StateVar}]
    };
normalize_expressions_for_provider(StateVar, {call, Line, {remote, Line1, {atom, Line2, liet},
                                                            {atom, Line3, get}}, [{atom, Line4, Name}|_]}) ->
    {call, Line,
     {remote, Line1, {atom, Line2, liet},
      {atom, Line3, get}},
      [{atom, Line4, Name},
       {var, Line4, StateVar}]
    };
normalize_expressions_for_provider(StateVar, Stmt) when is_tuple(Stmt) ->
    list_to_tuple(normalize_expressions_for_provider(StateVar, tuple_to_list(Stmt)));
normalize_expressions_for_provider(StateVar, Stmt) when is_list(Stmt) ->
    [normalize_expressions_for_provider(StateVar, S) || S <- Stmt];
normalize_expressions_for_provider(_StateVar, Stmt) ->
    Stmt.

find_deps({'fun', _Line, {clauses, Clauses}}) ->
    find_deps_in_clauses([], Clauses).

find_deps_in_clauses(Acc, []) ->
    lists:reverse(Acc);
find_deps_in_clauses(Acc, [{clause, _Line, _Vars, _Guards, Expressions}|T]) ->
    Acc2 = find_liet_refs_in_expressions(Acc, Expressions),
    find_deps_in_clauses(Acc2, T).

%% @doc finds [Ref...] in liet:Ref() and liet:get(Ref, _)
%% We must support both calling convetions so that the ast normalization can discover
%% uses of the State var and so that we can traverse the normalized ast afterward.
find_liet_refs_in_expressions(Acc, []) ->
    Acc;
find_liet_refs_in_expressions(Acc, {call, _Line, {remote, _Line1, {atom, _Line2, liet},
                                  {atom, _Line3, get}}, [{atom, Line4, Name}|_]}) ->
    [{atom, Line4, Name}|Acc];
find_liet_refs_in_expressions(Acc, {call, _Line, {remote, _Line1, {atom, _Line2, liet},
                                  {atom, Line3, Name}}, []}) ->
    [{atom, Line3, Name}|Acc];
find_liet_refs_in_expressions(Acc, Stmt) when is_tuple(Stmt) ->
    find_liet_refs_in_expressions(Acc, tuple_to_list(Stmt));
find_liet_refs_in_expressions(Acc, Stmt) when is_list(Stmt) ->
    lists:foldl(fun(S, Acc0) -> find_liet_refs_in_expressions(Acc0, S) end, Acc, Stmt);
find_liet_refs_in_expressions(Acc, _Stmt) ->
    Acc.

list_to_cons([], Line) -> {nil, Line};
list_to_cons([H|T], Line) ->
    {cons, Line, H, list_to_cons(T, Line)}.

unique_deps([], Acc) ->
    maps:values(Acc);
unique_deps([H={atom, _Line, Atom}|T], Acc) ->
    case maps:get(Atom, Acc, undefined) of
        undefined ->
            unique_deps(T, Acc#{Atom => H});
        _ ->
            unique_deps(T, Acc)
    end.
