-module(liet).

-export([apply/2, apply/3,
         destroy/3, destroy/4,
         compile/1,
         compile_file/1,
         get/2,
         task/1, task/2, task/3,
         linear_tasks/1]).

compile(_) -> {error, "If you see this message, you should add `{parse_transform, liet_transform}` to your erl_opts."}.

compile_file(File) ->
    {ok, Contents} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Contents)),
    {ok, [AST]} = erl_parse:parse_exprs(Tokens),
    TransformedAST = liet_transform:compile(AST),
    {value, Result, _} = erl_eval:exprs([TransformedAST], []),
    Result.

linear_tasks(Proplist) ->
    #{map := LMap} = lists:foldl(
      fun(Tuple, Acc=#{map := Map, deps := Deps}) ->
              {Name, Task} = case Tuple of
                                 {N, F, A} -> {N, task(F, A, Deps)};
                                 {N, F} -> {N, task(F, Deps)}
                             end,
              Map2 = Map#{Name => Task},
              Deps2 = [Name|Deps],
              Acc#{map => Map2,
                   deps => Deps2}
      end, #{map => #{}, deps => []}, Proplist),
    LMap.

task(F) -> task(F, []).
task(F, D) -> task(F, #{}, D).
task(F, A, D) -> #{apply => F, args => A, deps => D}.

apply(Map, Timeout) ->
    do_action(apply, Map, undefined, all, Timeout).

apply(Map, Targets, Timeout) ->
    do_action(apply, Map, undefined, Targets, Timeout).

destroy(Map, State, Timeout) ->
    Targets = case State of
                  undefined -> all;
                  State when is_map(State) -> maps:keys(State)
              end,
    do_action(destroy, Map, State, Targets, Timeout).

destroy(Map, State, Targets, Timeout) ->
    do_action(destroy, Map, State, Targets, Timeout).

do_action(Action, Map, Input, Targets, Timeout) ->
    {ok, Receiver} = liet_wrek_event_handler:start_link(),

    Map2 = wrektify(Action, Map, Input),
    Map3 = case Action of destroy -> reverse_deps(Map2); _ -> Map2 end,
    Map4 = filter_by_targets(Map3, Targets),
    {ok, _WrekPid} = wrek:start(Map4, [{event_manager, Receiver}]),

    Result = liet_wrek_event_handler:await(Receiver, Timeout),

    Result.

filter_by_targets(Map, all) -> Map;
filter_by_targets(Map, Targets) ->
    AllNames = get_visited_nodes_by_targets(Map, Targets, Targets),
    maps:filter(
      fun(VertName, _Value) -> lists:member(VertName, AllNames) end,
      Map).

get_visited_nodes_by_targets(_Map, [], Acc) ->
    Acc;
get_visited_nodes_by_targets(Map, [H|T], Acc) ->
    #{deps := Deps} = maps:get(H, Map),
    NextAcc = get_visited_nodes_by_targets(Map, Deps, []),
    Acc2 = lists:usort(Acc ++ Deps ++ NextAcc),
    get_visited_nodes_by_targets(Map, T, Acc2).

wrektify(apply, Vert=#{apply := Func, args := Args}, DefaultArgs) ->
    Vert2 = maps:without([apply, destroy, args], Vert),
    Vert2#{module => anon_vert,
           args => #{func => Func, args => select_args(Args, DefaultArgs)}};
wrektify(destroy, Vert=#{destroy := Func, args := Args}, DefaultArgs) ->
    Vert2 = maps:without([apply, destroy, args], Vert),
    Vert2#{module => anon_vert,
           args => #{func => Func, args => select_args(Args, DefaultArgs)}};
wrektify(Action, Map, DefaultArgs) when is_map(Map) ->
    maps:map(fun(_K, V) -> wrektify(Action, V, DefaultArgs) end, Map);
wrektify(_Action, X, _DefaultArgs) ->
    X.

select_args(undefined, DefaultArgs) -> DefaultArgs;
select_args(Args, _) -> Args.

%% @doc reverses dependency edges
reverse_deps(Map) ->
    List = maps:to_list(Map),
    Rev = lists:foldl(
      fun({Name, #{deps := Deps}}, Rev1) ->
              lists:foldl(
                fun(DName, Rev2) ->
                        RevDeps = maps:get(DName, Rev2, []),
                        Rev2#{DName => lists:usort([Name|RevDeps])}
                end, Rev1, Deps)
      end, #{}, List),
    Map2 = maps:map(
      fun(Name, WrekData) ->
              WrekData#{deps => maps:get(Name, Rev, [])}
      end, Map),
    Map2.

get(Name, Parent) ->
    wrek_vert:get(Parent, Name, result).
