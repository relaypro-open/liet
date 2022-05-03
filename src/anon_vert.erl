-module(anon_vert).
-behaviour(wrek_vert).

-export([run/2]).

-define(OkColor, "\e[32m").     % Green
-define(InfoColor, "\e[36m").   % Cyan
-define(ErrorColor, "\e[31m").  % Red
-define(UnsetColor, "\e[0m").   % Unset
-define(CheckMark, [10003]).

run(#{action := Action,
      name := Name,
      func := Fun,
      args := Args}, Parent) ->

    Debug = application:get_env(liet, debug, undefined),
    Logger = slog(Debug, Action, Name),

    Tick = erlang:monotonic_time(microsecond),
    Result = Fun(Args, Parent),
    Tock = erlang:monotonic_time(microsecond),

    elog(Debug, Logger, Action, Name, Tock-Tick),

    {ok, #{result => Result}}.

slog(undefined, _, _) ->
    ok;
slog(Debug, apply, Name) ->
    write_log(Debug, io_lib:format("[ ] Creating ~s~p~s...~n", [?InfoColor, Name, ?UnsetColor])),
    spawn_link(fun() -> logger(Debug, apply, Name) end);
slog(Debug, destroy, Name) ->
    write_log(Debug, io_lib:format("[ ] Destroying ~s~p~s...~n", [?InfoColor, Name, ?UnsetColor])),
    spawn_link(fun() -> logger(Debug, apply, Name) end).

elog(undefined, _Logger, _Action, _Name, _Usec) ->
    ok;
elog(Debug, Logger, apply, Name, Usec) ->
    Logger ! stop,
    write_log(Debug, io_lib:format("[~s~ts~s] Finished creating ~s~p~s (~p us)~n", [?OkColor, ?CheckMark, ?UnsetColor, ?OkColor, Name, ?UnsetColor, Usec]));
elog(Debug, Logger, destroy, Name, Usec) ->
    Logger ! stop,
    write_log(Debug, io_lib:format("[~s~ts~s] Finished destroying ~s~p~s (~p us)~n", [?OkColor, ?CheckMark, ?UnsetColor, ?OkColor, Name, ?UnsetColor, Usec])).

logger(Debug, Action, Name) ->
    fun Loop() ->
    receive
        stop ->
            ok
    after 1000 ->
              ilog(Debug, Action, Name),
              Loop()
    end
    end().

ilog(Debug, apply, Name) ->
    write_log(Debug, io_lib:format("[~s-~s] Still creating ~s~p~s...~n", [?InfoColor, ?UnsetColor, ?InfoColor, Name, ?UnsetColor]));
ilog(Debug, destroy, Name) ->
    write_log(Debug, io_lib:format("[~s-~s] Still destroying ~s~p~s...~n", [?InfoColor, ?UnsetColor, ?InfoColor, Name, ?UnsetColor])).

write_log(user, String) ->
    io:format(user, "~ts", [String]).
