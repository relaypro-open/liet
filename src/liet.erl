-module(liet).

-export([apply/2, apply/3,
         destroy/3, destroy/4,
         get/3]).

-define(LietInternalKey, '#liet-internal').

apply(Graph, Timeout) ->
    do_action(apply, Graph, undefined, all, Timeout).

apply(Graph, Targets, Timeout) ->
    do_action(apply, Graph, undefined, Targets, Timeout).

destroy(Graph, State, Timeout) ->
    Targets = case State of
                  undefined -> all;
                  State when is_map(State) -> maps:keys(maps:without([?LietInternalKey], State))
              end,
    do_action(destroy, Graph, State, Targets, Timeout).

destroy(Graph, State, Targets, Timeout) ->
    do_action(destroy, Graph, State, Targets, Timeout).

do_action(Action, Graph, Input, Targets, Timeout) ->
    {ok, Receiver} = liet_wrek_event_handler:start_link(),

    Map = if is_map(Graph) ->
                 Graph;
             true ->
                 {ok, GraphMap} = Graph:'#graph-'(),
                 GraphMap
          end,
    Map2 = wrektify(Action, Map, Input),
    Map3 = case Action of destroy -> reverse_deps(Map2); _ -> Map2 end,
    Map4 = filter_by_targets(Map3, Targets),

    case map_size(Map4) of
        0 ->
            {ok, #{}};
        _ ->
            InputRunnerSup = if is_map(Input) ->
                   maps:get(runner_sup, maps:get(?LietInternalKey, Input, #{}), undefined);
               true ->
                   undefined
            end,
            {ok, {_WrekPid, RunnerSup}} = wrek:start_link(Map4, [{event_manager, Receiver},
                                                                 {global_timeout, Timeout},
                                                                 {runner_sup, InputRunnerSup}]),
            case liet_wrek_event_handler:await(Receiver, Timeout) of
                {ok, Result} ->
                    {ok, Result#{?LietInternalKey => #{runner_sup => RunnerSup}}};
                Error ->
                    Error
            end
    end.

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

select_args(undefined, DefaultArgs) when is_map(DefaultArgs) -> maps:without([?LietInternalKey], DefaultArgs);
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

get(Name, Arg, State) when is_map(Arg) ->
    case maps:find(Name, Arg) of
        {ok, Value} ->
            Value;
        _ ->
            get(Name, undefined, State)
    end;
get(Name, _, State) ->
    wrek_vert:get(State, Name, result).
