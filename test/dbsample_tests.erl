-module(dbsample_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Timeout, 2000).

setup(Targets) ->
    {ok, State} = liet:apply(dbsample_lsg, Targets, ?Timeout),
    State.

teardown(State) ->
    liet:destroy(dbsample_lsg, State, ?Timeout).

%% API -- This is our stand-in for some real code that might
%% be interesting to test.
render(Table, State) -> ets:tab2list(maps:get(Table, State)).

%% Single post, single author
singlepost_test_() ->
    {setup,
     fun() -> setup([post_without_comments]) end,
     fun teardown/1,
     fun(State) ->
             [
              ?_assertEqual([{maps:get(jack, State),
                              "Jack",
                              "Sparrow"}], render(dbsample_authors, State)),
              ?_assertEqual([{maps:get(post_without_comments, State),
                              maps:get(jack, State),
                              "Welcome to my blog with no comments!",
                              []}], render(dbsample_posts, State))
             ]
     end}.

%% Multiple posts, still single author
multipost_test_() ->
    {setup,
     fun() -> setup(all) end,
     fun teardown/1,
     fun(State) ->
             [
              ?_assertMatch([_,_], render(dbsample_posts, State)),
              ?_assertEqual([{maps:get(jack, State),
                              "Jack",
                              "Sparrow"}], render(dbsample_authors, State))
             ]
     end}.
