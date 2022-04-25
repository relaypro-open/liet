%% @doc Demonstrates 2 methods for creating fixtures for testing
-module(dbsample_lsg).

-compile({parse_transform, liet_resource_graph}).

-export([create_fixture_without_liet/2]).

%% =============================================================================
%% Fixture creation without using Liet.
%%
%% Multiple clauses are used to represent the different application states our
%% fixture must implement. This approach can lead to code duplication or complex
%% logic. In this example, we're using code duplication.
%%
%% Additionally, we've coupled our test code with our fixture. The fixture needs
%% to be "aware" of the possible disparate states needed for testing.
%%
%% We omitted any cleanup calls for brevity.
%% =============================================================================
create_fixture_without_liet(all, Vars) ->
    #{author_name := {First, Last},
      post_content_nocomments := Post1,
      post_content_comments := Post2} = Vars,

    Posts = ets:new(dbsample_posts, [public]),
    Comments = ets:new(dbsample_comments, [public]),
    Authors = ets:new(dbsample_authors, [public]),
    Author = insert_new(Authors, {undefined, First, Last}),
    FirstComment = insert_new(Comments, {undefined, "First!!!"}),

    PostWithoutComments = insert_new(Posts, {undefiend, Author, Post1, []}),
    PostWithComments = insert_new(Posts, {undefiend, Author, Post2, [FirstComment]}),

    #{author => Author,
      dbsample_authors => Authors,
      dbsample_posts => Posts,
      dbsample_comments => Comments,
      first_comment => FirstComment,
      post_without_comments => PostWithoutComments,
      post_with_comments => PostWithComments};
create_fixture_without_liet([post_without_comments], Vars) ->
    #{author_name := {First, Last},
      post_content_nocomments := Post1} = Vars,

    Posts = ets:new(dbsample_posts, [public]),
    Comments = ets:new(dbsample_comments, [public]),
    Authors = ets:new(dbsample_authors, [public]),
    Author = insert_new(Authors, {undefined, First, Last}),

    PostWithoutComments = insert_new(Posts, {undefiend, Author, Post1, []}),

    #{author => Author,
      dbsample_authors => Authors,
      dbsample_posts => Posts,
      dbsample_comments => Comments,
      post_without_comments => PostWithoutComments}.

%% =============================================================================
%% Fixture creation with Liet.
%%
%% The functions below define resources that are created using a depedendency graph.
%% This ensures that resources are created exactly once and only when required.
%%
%% The test code specifies which path through the graph is required to create
%% the particular application state needed for that test. So, there is no coupling
%% between the test code and the fixture.
%%
%% There is no code duplication. Also, it's easy to define 'destroy' functions
%% that automatically clean up the created resources.
%% =============================================================================
dbsample_posts()    -> ets:new(dbsample_posts, [public]).
dbsample_comments() -> ets:new(dbsample_comments, [public]).
dbsample_authors()  -> ets:new(dbsample_authors, [public]).

author_name() -> {"", ""}.
post_content_nocomments() -> "".
post_content_comments() -> "".
comment_content() -> "First!".

post_without_comments() ->
    insert_new(dbsample_posts(),
               {undefined, author(), post_content_nocomments(), []}).

post_with_comments() ->
    insert_new(dbsample_posts(),
               {undefined, author(), post_content_comments(), [first_comment()]}).

first_comment() ->
    insert_new(dbsample_comments(),
               {undefined, comment_content()}).

author() ->
    {First, Last} = author_name(),
    insert_new(dbsample_authors(),
               {undefined, First, Last}).

dbsample_posts(destroy)    -> ets:delete(dbsample_posts()).
dbsample_comments(destroy) -> ets:delete(dbsample_comments()).
dbsample_authors(destroy)  -> ets:delete(dbsample_authors()).

post_without_comments(destroy) -> ets:delete(dbsample_posts(),    post_without_comments()).
post_with_comments(destroy)    -> ets:delete(dbsample_posts(),    post_with_comments()).
first_comment(destroy)         -> ets:delete(dbsample_comments(), first_comment()).
author(destroy)                  -> ets:delete(dbsample_authors(),  author()).

%% =============================================================================
%% Internal functions
%% =============================================================================
insert_new(Table, Tuple) ->
    Id = erlang:unique_integer(),
    ets:insert(Table, setelement(1, Tuple, Id)),
    Id.
