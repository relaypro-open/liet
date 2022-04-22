-module(true_vert).
-behaviour(wrek_vert).

-export([run/2]).

run(_Args, Parent) ->
    {ok, Fun} = wrek_vert:exec(Parent, ".", "true"),
    ok = Fun(),
    {ok, #{}}.
