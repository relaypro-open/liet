-module(dbsample_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Timeout, 2000).

setup(Targets) ->
    ets:new(dbsample_posts, [public, named_table]),
    ets:new(dbsample_comments, [public, named_table]),
    ets:new(dbsample_authors, [public, named_table]),
    {ok, State} = liet:apply(dbsample_lsg, Targets, ?Timeout),
    State.

teardown(State) ->
    liet:destroy(dbsample_lsg, State, ?Timeout),
    ets:delete(dbsample_authors),
    ets:delete(dbsample_comments),
    ets:delete(dbsample_posts).

%% API -- This is our stand-in for some real code that might
%% be interesting to test.
render(Table) -> ets:tab2list(Table).

%% Single post, single author
singlepost_test_() ->
    {setup,
     fun() -> setup([post_without_comments]) end,
     fun teardown/1,
     fun(State) ->
             [
              ?_assertEqual([{maps:get(jack, State),
                              "Jack",
                              "Sparrow"}], render(dbsample_authors)),
              ?_assertEqual([{maps:get(post_without_comments, State),
                              maps:get(jack, State),
                              "Welcome to my blog with no comments!",
                              []}], render(dbsample_posts))
             ]
     end}.

%% Multiple posts, still single author
multipost_test_() ->
    {setup,
     fun() -> setup(all) end,
     fun teardown/1,
     fun(State) ->
             [
              ?_assertMatch([_,_], render(dbsample_posts)),
              ?_assertEqual([{maps:get(jack, State),
                              "Jack",
                              "Sparrow"}], render(dbsample_authors))
             ]
     end}.
