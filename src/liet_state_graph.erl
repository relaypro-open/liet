-module(liet_state_graph).

-export([parse_transform/2]).

-define(GraphFuncName, '#graph-').
-define(ArgVar, 'ArgVar@@').
-define(StateVar, 'StateVar@@').

parse_transform(AST, _Options) ->
    {Resources, AST2} = walk_ast(#{}, [], AST),

    Line = 1000,

    Noop = create_anon_vert_function([{atom, Line, ok}]),
    ResourcesAssoc = lists:map(
      fun({ResourceName, ASTMetadata=#{deps := Deps}}) ->
        % the resource apply and destroy functions have been transformed into functions
        % that return arity-2 anonymous functions compatible with anon_vert, so we can 
        % simple make the call to the module function rather than tracking the full AST
        Apply = case maps:get(apply, ASTMetadata, false) of
                    true -> {call, Line, {atom, Line, ResourceName}, []};
                    false -> Noop
                end,
        Destroy = case maps:get(destroy, ASTMetadata, false) of
                      true -> {call, Line, {atom, Line, ResourceName}, [{atom, Line, destroy}]};
                      false -> Noop
                  end,
        TaskMap = {map, Line,
                   [{map_field_assoc, Line, {atom, Line, deps}, atom_list_to_cons(Deps, Line)},
                   {map_field_assoc, Line, {atom, Line, apply}, Apply},
                   {map_field_assoc, Line, {atom, Line, destroy}, Destroy},
                   {map_field_assoc, Line, {atom, Line, args}, {atom, Line, undefined}}]},
        Resource = {map_field_assoc, Line, {atom, Line, ResourceName}, TaskMap},
        Resource
      end, maps:to_list(Resources)),

    GraphBody = [{tuple, Line, [{atom, Line, ok}, {map, Line, ResourcesAssoc}]}],
    GraphFunction = {function, Line, '#graph-', 0, [{clause, Line, [], [], GraphBody}]},

    AST3 = add_to_exports([], AST2 ++ [GraphFunction, {eof, Line+1}], [{?GraphFuncName, 0}]),
    AST3.

add_to_exports(Acc, [], _) ->
    lists:reverse(Acc);
add_to_exports(Acc, [{attribute, Line, export, Exports}|T], NewExports) ->
    add_to_exports([{attribute, Line, export, lists:usort(Exports ++ NewExports)}|Acc], T, done);
add_to_exports(Acc, AST=[{function, Line, _Name, _Arity, _Clauses}|_], NewExports) when is_list(NewExports) ->
    add_to_exports([{attribute, Line, export, NewExports}|Acc], AST, done);
add_to_exports(Acc, [H|T], NewExports) ->
    add_to_exports([H|Acc], T, NewExports).

walk_ast(Resources, Acc, []) ->
    {Resources, lists:reverse(Acc)};
walk_ast(Resources, Acc, [{attribute, _, module, _Module}=H|T]) ->
    walk_ast(Resources, [H|Acc], T);

%% resource function -- 0 arity, single clause
walk_ast(Resources, Acc, [{function, Line, Name, 0, [{clause, _Line1, Args=[], [], ApplyBody}]}|T]) ->
    {Resources2, Acc2} = walk_apply_destroy(Resources, Acc, Line, Name, apply, Args, ApplyBody),
    walk_ast(Resources2, Acc2, T);

%% destroy function -- 1 arity, single clause, argument is the atom=destroy
walk_ast(Resources, Acc, [{function, Line, Name, 1, [{clause, _Line1, Args=[{atom, _Line2, destroy}], [], DestroyBody}]}|T]) ->
    {Resources2, Acc2} = walk_apply_destroy(Resources, Acc, Line, Name, destroy, Args, DestroyBody),
    walk_ast(Resources2, Acc2, T);

walk_ast(Resources, Acc, [{eof, _}|T]) ->
    walk_ast(Resources, Acc, T);

walk_ast(Resources, Acc, [H|T]) ->
    walk_ast(Resources, [H|Acc], T).

walk_apply_destroy(Resources, Acc, Line, Name, ApplyOrDestroy, Args, Body) ->

    NBody = create_anon_vert_function(Body),
    DestroyDeps = find_deps(NBody) -- [Name],
    Function = {function, Line, Name, length(Args), [{clause, Line, Args, [], [NBody]}]},

    Resource = #{deps := Deps} = maps:get(Name, Resources, #{deps => []}),
    Resources2 = Resources#{Name => Resource#{ApplyOrDestroy => true, deps => lists:usort(Deps ++ DestroyDeps)}},

    {Resources2, [Function|Acc]}.

%% turns any body into an arity-2 anonymous function
create_anon_vert_function(Body=[H|_]) ->
    Line = element(2, H),
    {Transformed, Body2} = rewrite_anon_vert_function_body(Body),

    % track whether or not the body was transformed with liet:get/3 so that we can
    % avoid unused variable warnings
    Vars = case Transformed of
               true ->
                   [{var, Line, ?ArgVar},
                    {var, Line, ?StateVar}];
               false ->
                   [{var, Line, '_'},
                    {var, Line, '_'}]
           end,

    Clauses = [{clause, Line,
                Vars,
                [], % guards
                Body2}],
    {'fun', Line,
     {clauses, Clauses}}.

rewrite_anon_vert_function_body({call, Line, {atom, Line1, Resource}, []}) ->
    %% We assume all calls to 0-arity functions are resource calls
    {true,
     {call, Line,
      {remote, Line1, {atom, Line1, liet},
       {atom, Line1, get}},
      [{atom, Line1, Resource},
       {var, Line1, ?ArgVar},
       {var, Line1, ?StateVar}]
     }};
rewrite_anon_vert_function_body(AST) when is_tuple(AST) ->
    {Transformed, AST2} = rewrite_anon_vert_function_body(tuple_to_list(AST)),
    {Transformed, list_to_tuple(AST2)};
rewrite_anon_vert_function_body(AST) when is_list(AST) ->
    {Transformed, AST2} = lists:unzip([ rewrite_anon_vert_function_body(X) || X <- AST ]),
    {lists:any(fun(X) -> X end, Transformed), AST2};
rewrite_anon_vert_function_body(AST) ->
    {false, AST}.

find_deps({'fun', _Line, {clauses, Clauses}}) ->
    find_deps_in_clauses([], Clauses).

find_deps_in_clauses(Acc, []) ->
    lists:reverse(Acc);
find_deps_in_clauses(Acc, [{clause, _Line, _Vars, _Guards, Expressions}|T]) ->
    Acc2 = find_liet_refs_in_expressions(Acc, Expressions),
    find_deps_in_clauses(Acc2, T).

find_liet_refs_in_expressions(Acc, []) ->
    Acc;
find_liet_refs_in_expressions(Acc, {call, _Line, {remote, _Line1, {atom, _Line2, liet},
                                  {atom, _Line3, get}}, [{atom, _Line4, Name}|_]}) ->
    [Name|Acc];
find_liet_refs_in_expressions(Acc, Stmt) when is_tuple(Stmt) ->
    find_liet_refs_in_expressions(Acc, tuple_to_list(Stmt));
find_liet_refs_in_expressions(Acc, Stmt) when is_list(Stmt) ->
    lists:foldl(fun(S, Acc0) -> find_liet_refs_in_expressions(Acc0, S) end, Acc, Stmt);
find_liet_refs_in_expressions(Acc, _Stmt) ->
    Acc.

atom_list_to_cons([], Line) -> {nil, Line};
atom_list_to_cons([H|T], Line) ->
    {cons, Line, {atom, Line, H}, atom_list_to_cons(T, Line)}.
