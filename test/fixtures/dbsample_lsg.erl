-module(dbsample_lsg).
-compile({parse_transform, liet_state_graph}).

post_without_comments() ->
    PostId = erlang:unique_integer(),
    ets:insert(dbsample_posts, {PostId, jack(), "Welcome to my blog with no comments!", []}),
    PostId.

post_without_comments(destroy) ->
    ets:delete(dbsample_posts, post_without_comments()).  
post_with_comments() ->
    PostId = erlang:unique_integer(),
    ets:insert(dbsample_posts, {PostId, jack(), "Welcome to my blog with comments!", [first_comment()]}),
    PostId.

post_with_comments(destroy) ->
    ets:delete(dbsample_posts, post_with_comments()).

first_comment() ->
    CommentId = erlang:unique_integer(),
    ets:insert(dbsample_comments, {CommentId, "First!!!"}),
    CommentId.

first_comment(destroy) ->
    ets:delete(dbsample_comments, first_comment()).

jack() ->
    AuthorId = erlang:unique_integer(),
    ets:insert(dbsample_authors, {AuthorId, "Jack", "Sparrow"}),
    AuthorId.
