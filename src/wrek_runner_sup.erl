-module(wrek_runner_sup).

-export([start_link/1, get_runner/2]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(Names :: list(atom())) -> {ok, pid()} | ignore | {error, term()}.

start_link(Names) ->
    {ok, Sup} = supervisor:start_link(?MODULE, []),
    [ supervisor:start_child(Sup,
                             #{id => X,
                               start => {wrek_vert_runner, start_link, [#{stop_on_completion => false}]},
                               restart => temporary,
                               type => worker})
      || X <- Names ],
    {ok, Sup}.

get_runner(Name, Sup) ->
    Children = supervisor:which_children(Sup),
    case lists:keyfind(Name, 1, Children) of
        false ->
            undefined;
        {Name, Runner, _, _} ->
            Runner
    end.

%% Callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 0,
      period => 1
     },
    {ok, {SupFlags, []}}.
