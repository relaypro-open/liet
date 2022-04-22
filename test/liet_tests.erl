-module(liet_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("liet/include/liet.hrl").

-define(Timeout, 2000).

liet_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(#{graph := LietGraph,
           meck := MeckGraph,
           stateful := StatefulGraph,
           single := SingleVisitGraph,
           recursive := RecursiveDepsGraph}) ->
             [
                %% Implicitly defined linear dependency graph -- AKA a very complicated fold
                ?_assertMatch({ok, #{root := hello,
                                   leaf := [hello, world]}},
                            liet:apply(
                              liet:linear_tasks(
                                [{root, fun(_, _State) -> hello end},
                                 {leaf, fun(_, State) -> [liet:get(root, State), world] end}]
                               ),
                              ?Timeout
                             )
                           )

                %% Explicitly defined nonlinear depedency graph
              , ?_assertMatch({ok, #{vfoo := foo,
                                   vbar := [foo, bar],
                                   vbaz := [foo, baz]}},
                            liet:apply(
                              #{vfoo => liet:task(fun(_, _State) -> foo end),
                                vbar => liet:task(fun(_, State) -> [liet:get(vfoo, State), bar] end, [vfoo]),
                                vbaz => liet:task(fun(_, State) -> [liet:get(vfoo, State), baz] end, [vfoo])
                               },
                              ?Timeout
                             )
                           )

                %% Implicitly defined nonlinear dependency graph
              , ?_assertMatch({ok, #{
                               root_dir    := "/",
                               var_dir     := "/var",
                               tmp_dir     := "/tmp",
                               var_log_dir := "/var/log"
                              }},
                            liet:apply(LietGraph, ?Timeout))

              , ?_assertMatch({ok, #{
                               root_dir    := ok,
                               var_dir     := ok,
                               tmp_dir     := ok,
                               var_log_dir := ok
                              }},
                            liet:destroy(LietGraph, undefined, ?Timeout))

                %% Meck test (with non-trivial destroy)
              , ?_assertMatch({ok, #{string := ok}}, liet:apply(MeckGraph, ?Timeout))
              , ?_assertMatch(bar, string:is_empty("foo"))
              , ?_assertMatch({ok, #{string := ok}}, liet:destroy(MeckGraph, undefined, ?Timeout))
              , ?_assertMatch(false, string:is_empty("foo"))

                %% Apply specific target. Tests: (a) runs successfully and (b) does not apply the tmp_dir
              , ?_assertMatch({ok, _}, liet:apply(LietGraph, [var_log_dir], ?Timeout))
              , ?_assertNotMatch({ok, #{tmp_dir := _}}, liet:apply(LietGraph, [var_log_dir], ?Timeout))

                %% Apply a graph and pass the State into the destroy
              , ?_assertMatch({{ok,#{my_env_entry := my_key}},{ok,#{my_env_entry := ok}}},
                              apply_and_destroy_env({my_key, foo}, StatefulGraph))

                %% Compile from file
              , ?_assertMatch({ok, #{}}, liet:compile_file("test/graph.liet"))
              , ?_assertMatch({ok, #{human := [think|_]}}, liet:apply(element(2, liet:compile_file("test/graph.liet")), ?Timeout))

                %% Ensure nodes are applied 1 time maximum
              , ?_assertMatch({ok, #{read := [{liet, 1}]}}, liet:apply(SingleVisitGraph, [read], ?Timeout))

                %% Ensure dep calculation is recursive
              , ?_assertMatch({ok, #{b := {ok, true}}}, liet:apply(RecursiveDepsGraph, ?Timeout))
              , ?_assertMatch({ok, _}, liet:destroy(RecursiveDepsGraph, undefined, ?Timeout))
             ]
     end}.

apply_and_destroy_env({ExpectKey, ExpectVal}, Graph) ->
    ApplyResult = {ok, State} = liet:apply(Graph, ?Timeout),
    {ok, ExpectVal} = application:get_env(liet, ExpectKey),
    DestroyResult = liet:destroy(Graph, State, ?Timeout),
    undefined = application:get_env(liet, ExpectKey),
    {ApplyResult, DestroyResult}.

setup() ->
    {ok, _} = application:ensure_all_started(meck),
    {ok, _} = application:ensure_all_started(liet),

    {ok, _} = liet:compile([]),

    %% Inside liet:compile, all calls to liet:Var() are replaced in the AST
    %% by calls to liet:get(Var, State). The AST compiler (liet_transform) also
    %% keeps track of dependencies through these calls, so that the author does
    %% not have to explicity define depdendencies or order of execution.
    %%
    %% Additons or removals of liet:Var() in each node will automatically be picked up by
    %% liet:compile
    %%
    %% The macro ?l is provided to help maintain syntax highlighting between the node names
    %% and the internal references. It expands to liet:X()
    {ok, LietGraph} = liet:compile(
                  [
                   % name        code
                   {var_log_dir, filename:join(?l(var_dir), "log")},
                   {tmp_dir,     fun(_Input) -> filename:join(?l(root_dir), "tmp") end},
                   {var_dir,     fun(_Input) -> filename:join(?l(root_dir), "var") end},
                   {root_dir,    "/"}
                  ]
                 ),

    {ok, MeckGraph} = liet:compile(
                  [
                   {string, fun() ->
                                    meck:new(string, [unstick, passthrough, no_link]),
                                    meck:expect(string, is_empty, fun("foo") -> bar end)
                            end, meck:unload(string)}
                  ]
                 ),

    {ok, StatefulGraph} = liet:compile(
                      [
                       {my_env_entry,
                        fun() ->
                                Key = my_key, application:set_env(liet, Key, foo),
                                Key
                        end,
                        fun(#{my_env_entry := Key}) ->
                                application:unset_env(liet, Key)
                        end
                       }
                      ]
                     ),

    %% Currently, we cannot define ets tables (or any linked process) as a node in a liet graph.
    %% The wrek library spawns new processes to execute each node, and they are short-lived.
    ets:new(liet_counter, [public, named_table]),

    %% This graph puts 2 dependencies on the atomic operation to update a counter. If 'incr' is
    %% visited more than once, the 'read' result will reflect that
    {ok, SingleVisitGraph} = liet:compile(
                               [
                                {incr, ets:update_counter(liet_counter, liet, 1, {liet, 0})},
                                {a, fun() -> _ = ?l(incr), ok end},
                                {b, fun() -> _ = ?l(incr), ok end},
                                {read, fun() -> _ = [?l(a), ?l(b)], ets:lookup(liet_counter, liet) end}
                               ]
                              ),

    %% Make sure b runs after root
    {ok, RecursiveDepsGraph} = liet:compile([
                                 {root, fun() -> timer:sleep(100), application:set_env(liet, root_finished, true) end, application:unset_env(liet, root_finished)},
                                 {a, fun() -> _ = ?l(root), ok end},
                                 {b, fun() -> _ = ?l(a), application:get_env(liet, root_finished) end}
                                ]),

    #{graph => LietGraph,
      meck => MeckGraph,
      stateful => StatefulGraph,
      single => SingleVisitGraph,
      recursive => RecursiveDepsGraph
     }.

teardown(#{}) ->
    ets:delete(liet_counter),
    ok.
