-module(meckstring).
-compile({parse_transform, liet_state_graph}).

string() ->
    meck:new(string, [unstick, passthrough, no_link]),
    meck:expect(string, is_empty, fun("foo") -> bar end).

string(destroy) ->
    meck:unload(string).
