liet
=====

Liet is an Erlang/OTP application that allows a developer to easily define and manage a complex
set of tasks embedded in a dependency graph without having to use nested data structures.

Liet was inspired by the Terraform core.

Please see 'Implicitly defined nonlinear dependency graph' in `test/liet_tests.erl` for examples.

Build
-----
    $ rebar3 compile

Test
----
    $ rebar3 eunit
