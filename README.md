Liet
=====

Liet is an Erlang/OTP application that allows a developer to easily define and
manage a complex set of tasks embedded in a dependency graph without having to
use nested data structures. Liet was inspired by the Terraform core.

As a developer, you will write a special Erlang module that represents your Liet
Resource Graph (LRG). When this module is compiled with
`{parse_transform, liet_resource_graph}`, your module can be used with `gen_liet`,
`liet:apply/N`, or`liet:destroy/N` to create and destroy stateful resources.

Why use Liet instead of regular functional programming? Liet provides the
following benefits:

1) Liet embraces side effects.
2) A resource will never be created or destroyed more than once.
3) All resources are destroyed automatically when the calling process exits.
4) Your LRG code can pull information seamlesslesly, without you having to plumb
through a `State` structure through function calls.
5) Resources are executed concurrently where possible.

Eunit Fixture Sample
--------------------
Please see `test/dbsample_tests.erl` and `test/dbsample_lrg.erl` for a
demonstration of the benefits of Liet for the creation of test fixtures.

Fixtures are usually written to create a specific application state rather than
perform some programmatic function. This makes them good candidates for Liet.

Resources
---------
A **Liet Resource** is a stateful entity that you, the developer, wish to keep
track of.  To create **Liet Resource**s, you must define a new Erlang module,
and compile with `{parse_transform, liet_resource_graph}`.

Example module `my_resources`:

```
%% my_resources.erl
-module(my_resources).
-compile({parse_transform, liet_resource_graph}).
```

In your module, you will define two functions to manage each resource: **Apply**
and **Destroy**.

The **Liet Resource Apply Function** MUST be defined. It does whatever
you choose, and is encouraged to execute code with side-effects. A function is
an **Apply Function** if it meets these requirements:

1) It has 0 arity
2) It has exactly 1 clause
3) It is not exported

Here's an example:

```
%% my_resources.erl
animals() -> ets:new(animals, [public]).
```

The **Liet Resource Destroy Function** SHOULD be defined when there is a
side-effect that you want to clean up. A function is a **Destroy Function** if
it meets these requirements:

1) It has 1 arity
2) It has exactly 1 clause
3) The argument in the clause matches the atom `destroy`
4) It is not exported

To continue our ets example:

```
%% my_resources.erl
animals(destroy) -> ets:delete(animals()).
```

The parse transform does not modify any functions other than those that meet the
**Apply** and **Destroy** requirements above.

As you may have noticed in the **Destroy** example, **Liet Resource Function**s can
include references to other resources. When a 0-arity call is defined inside
a resource function implementation, it is not a normal function call. Rather, it
is an accessor to the **Liet Runtime State**, discussed below.

To conclude our ets example in module `my_resources`:

```
%% my_resources.erl
dog() -> ets:insert(animals(), {dog, bark}).
cat() -> ets:insert(animals(), {cat, meow}).
```

To execute your **Liet Resource Graph**:

```
{ok, Pid} = gen_liet:start_link(my_resources).
```

This creates the **Liet Runtime State**. Liet guarantees that each resource
is created exactly once. The state keeps track of only the return value
of each **Apply**. It can be retrieved with:

```
State = gen_liet:get_state(Pid).
```

When the process referenced by `Pid` exits, Liet will automatically call all
**Destroy**s that are required, and in the correct order (reverse).

Targeting
---------
A resource graph can be reduced in size before execution by specifying
`targets`. By passing in a list of resource names, Liet will filter the 
resource graph down to only the resources required to create the target resources.

```
%% The 'cat' resource is not created
{ok, _} = gen_liet:start_link(my_resources, #{targets => [dog]}).
```

Vars
----
When executing a resource graph, you can influence the data at runtime by overriding
resources with specific values. This is done via the `vars` parameter.

```
%% The ets table called 'animals' is not created
MyTable = ets:new(my_table, [public, named_table]),
{ok, _} = get_liet:start_link(my_resources, #{vars => #{animals => MyTable}}).
```

Parse Transform
---------------
The implementations of the **Apply**s and **Destroy**s are traversed by
`liet_resource_graph` at compile time to:

1) Determine dependencies
2) Replace calls to resources with new calls that are accessors to the
**Liet Runtime State**.

The final result of `liet_resource_graph` is an additional exported function on your module
called `'#graph-'/0`. This function is used internally by Liet to kick off the creation
and destruction of resources. You shouldn't call this function.

Build
-----
    $ rebar3 compile

Test
----
    $ rebar3 eunit
