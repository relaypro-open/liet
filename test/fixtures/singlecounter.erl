-module(singlecounter).
-compile({parse_transform, liet_state_graph}).

%% This graph puts 2 dependencies on the atomic operation to update a counter. If 'incr' is
%% visited more than once, the 'read' result will reflect that

t() ->
    ets:new(liet_counter, [public]).
t(destroy) ->
    ets:delete(t()).

incr() ->
    ets:update_counter(t(), liet, 1, {liet, 0}).

a() ->
    _ = incr(),
    ok.

b() ->
    _ = incr(),
    ok.

read() -> 
    _ = [a(), b()],
    ets:lookup(t(), liet).
