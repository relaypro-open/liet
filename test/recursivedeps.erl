-module(recursivedeps).
-compile({parse_transform, liet_resource_graph}).

%% Make sure b runs after root

root() ->
    timer:sleep(100),
    application:set_env(liet, root_finished, true).

a() ->
    _ = root(),
    ok.

b() ->
    _ = a(),
    application:get_env(liet, root_finished).
