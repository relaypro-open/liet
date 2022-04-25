-module(myenv).
-compile({parse_transform, liet_resource_graph}).

%% We return the Key name from this resource so that it
%% can be referenced in the destroy.
my_env_entry() ->
    Key = my_key,
    application:set_env(liet, Key, foo),
    Key.

my_env_entry(destroy) ->
    application:unset_env(liet, my_env_entry()).
