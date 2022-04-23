liet
=====

Liet is an Erlang/OTP application that allows a developer to easily define and manage a complex
set of tasks embedded in a dependency graph without having to use nested data structures.

Liet was inspired by the Terraform core.

As a developer, you will write a special Erlang module that represents your Liet State Graph (LSG).
When this module is compiled with `{parse_transform, liet_state_graph}`, your module can be used
with `liet:apply/N` and `liet:destroy/N` to create and destroy stateful resources.

Why use Liet instead of bare function calls? Liet provides the following benefits:

1) A resource will never be created or destroyed more than once.
2) All resources can be destroyed with a single function call.
3) Your LSG code can pull information seamlesslesly, without you having to plumb through a
   `State` structure through function calls.
4) Resources are executed concurrently where possible.

Build
-----
    $ rebar3 compile

Test
----
    $ rebar3 eunit
