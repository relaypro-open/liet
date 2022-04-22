%%%-------------------------------------------------------------------
%% @doc liet public API
%% @end
%%%-------------------------------------------------------------------

-module(liet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    liet_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
