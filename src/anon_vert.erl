-module(anon_vert).
-behaviour(wrek_vert).

-export([run/2]).

run(#{func := Fun, args := Args}, Parent) ->
    Result = Fun(Args, Parent),
    {ok, #{result => Result}}.
