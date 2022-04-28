-module(linuxdirs).
-compile({parse_transform, liet_resource_graph}).

var_log_dir() -> filename:join(var_dir(), "log").

tmp_dir() -> filename:join(root_dir(), "tmp").

var_dir() -> filename:join(root_dir(), "var").

root_dir() -> "/".
