-module(dbsample_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Timeout, 2000).

%% API -- This is our stand-in for some real code that might
%% be interesting to test.
render(Table, State) -> ets:tab2list(maps:get(Table, State)).

fixture(Targets) ->
    {ok, Pid} = gen_liet:start_link(dbsample_lsg, #{targets => Targets,
                                                    apply_timeout => ?Timeout}),
    gen_liet:get_state(Pid, ?Timeout).

%% Single post, single author
singlepost_test() ->
    State = #{jack := JackId,
              post_without_comments := PostId} = fixture([post_without_comments]),

    ?assertEqual([{JackId, "Jack", "Sparrow"}], render(dbsample_authors, State)),
    ?assertEqual([{PostId, JackId, "Welcome to my blog with no comments!", []}],
                 render(dbsample_posts, State)).

%% Multiple posts, still single author
multipost_test() ->
    State = #{jack := JackId} = fixture(all),

    ?assertEqual([{JackId, "Jack", "Sparrow"}], render(dbsample_authors, State)),
    ?assertMatch([_, _], render(dbsample_posts, State)).
