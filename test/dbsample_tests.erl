-module(dbsample_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Timeout, 2000).

-define(UseLiet, true).

-define(AuthorFirst,           "Jack").
-define(AuthorLast,            "Sparrow").
-define(PostContentNoComments, "No commments here").
-define(PostContentComments,   "Comments are welcome").

-define(Vars, #{author_name             => {?AuthorFirst, ?AuthorLast},
                post_content_nocomments => ?PostContentNoComments,
                post_content_comments   => ?PostContentComments}).

%% API -- This is our stand-in for some real code that might
%% be interesting to test.
render(Table, State) -> ets:tab2list(maps:get(Table, State)).

fixture(true, Targets) ->

    {ok, Pid} = gen_liet:start_link(dbsample_lsg, #{targets => Targets,
                                                    vars => ?Vars,
                                                    apply_timeout => ?Timeout}),
    gen_liet:get_state(Pid, ?Timeout);
fixture(false, Targets) ->
    dbsample_lsg:create_fixture_without_liet(Targets, ?Vars).

%% Single post, single author
singlepost_test() ->
    State = #{author := AuthorId,
              post_without_comments := PostId} = fixture(?UseLiet, [post_without_comments]),

    ?assertEqual([{AuthorId, ?AuthorFirst, ?AuthorLast}], render(dbsample_authors, State)),
    ?assertEqual([{PostId, AuthorId, ?PostContentNoComments, []}], render(dbsample_posts, State)).

%% Multiple posts, still single author
multipost_test() ->
    State = #{author := AuthorId} = fixture(?UseLiet, all),

    ?assertEqual([{AuthorId, ?AuthorFirst, ?AuthorLast}], render(dbsample_authors, State)),
    ?assertMatch([_, _], render(dbsample_posts, State)).
