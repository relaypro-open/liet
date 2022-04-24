-module(dbsample_lsg).
-compile({parse_transform, liet_state_graph}).

dbsample_posts()    -> ets:new(dbsample_posts, [public]).
dbsample_comments() -> ets:new(dbsample_comments, [public]).
dbsample_authors()  -> ets:new(dbsample_authors, [public]).

post_without_comments() ->
    insert_new(dbsample_posts(),
               {undefined, jack(), "Welcome to my blog with no comments!", []}).

post_with_comments() ->
    insert_new(dbsample_posts(),
               {undefined, jack(), "Welcome to my blog with comments!", [first_comment()]}).

first_comment() ->
    insert_new(dbsample_comments(),
               {undefined, "First!!!"}).

jack() ->
    insert_new(dbsample_authors(),
               {undefined, "Jack", "Sparrow"}).

dbsample_posts(destroy)    -> ets:delete(dbsample_posts()).
dbsample_comments(destroy) -> ets:delete(dbsample_comments()).
dbsample_authors(destroy)  -> ets:delete(dbsample_authors()).

post_without_comments(destroy) -> ets:delete(dbsample_posts(),    post_without_comments()).
post_with_comments(destroy)    -> ets:delete(dbsample_posts(),    post_with_comments()).
first_comment(destroy)         -> ets:delete(dbsample_comments(), first_comment()).
jack(destroy)                  -> ets:delete(dbsample_authors(),  jack()).

insert_new(Table, Tuple) ->
    Id = erlang:unique_integer(),
    ets:insert(Table, setelement(1, Tuple, Id)),
    Id.
