-module(wrek_vert_runner).

-export([start_link/1,
         set_stop_on_completion/2,
         run/5,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% #{stop_on_completion => true (default) | false}
-spec start_link(#{atom() => term()}) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec set_stop_on_completion(pid(), boolean()) -> ok.
set_stop_on_completion(Pid, Flag) ->
    gen_server:cast(Pid, {set, stop_on_completion, Flag}).

-spec run(pid(), function(), map(), pid(), timeout()) -> any().
run(Pid, Fun, Args, Parent, Timeout) ->
    gen_server:call(Pid, {run, Fun, Args, Parent}, Timeout).

-spec init(#{}) -> {ok, #{}}.
init(#{}=Args) ->
    {ok, Args}.

-spec handle_call(term(), term(), map()) -> {stop, term(), term(), map()} | {reply, term(), map()}.
handle_call({run, Fun, Args, Parent}, _From, State) ->
    Result = Fun(Args, Parent),
    case maps:get(stop_on_completion, State, true) of
        true ->
            {stop, normal, Result, State};
        false ->
            {reply, Result, State}
    end.

-spec handle_cast({set, atom(), term()}, map()) -> {noreply, map()}.
handle_cast({set, Key, Var}, State) ->
    {noreply, State#{Key => Var}}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extras) ->
    {ok, State}.
