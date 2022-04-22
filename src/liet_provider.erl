-module(liet_provider).

-callback parse_apply(AST :: any()) ->
    AST :: any().
-callback parse_destroy(AST :: any()) ->
    AST :: any().
