-module(foodchain).
-compile({parse_transform, liet_state_graph}).

%% Exported functions are not tracked as liet resources
-export([sunlight/0]).

%% Macros can embed resource calls. Notice the call to t() here
-define(Ins(X), begin true = ets:insert(t(), {X}), X end).

%% =============================================================================
%% Module API
%% =============================================================================

sunlight() -> [shine].

%% =============================================================================
%% Liet Resources
%% =============================================================================

t() -> ets:new(?MODULE, [public, named_table, duplicate_bag]).
t(destroy) -> ets:delete(t()).

grass() -> ?Ins([grow|sunlight()]).

cow() -> ?Ins([moo|grass()]).

rabbit() -> ?Ins([hop|grass()]).

dog() -> ?Ins([bark|rabbit()]).

human() -> ?Ins([think|lists:usort(rabbit() ++ cow())]).
