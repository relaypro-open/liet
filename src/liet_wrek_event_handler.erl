-module(liet_wrek_event_handler).
-include_lib("wrek/include/wrek_event.hrl").

-export([start_link/0, await/2,
         code_change/3,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2]).
-behaviour(gen_event).

start_link() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, ?MODULE, #{}),
    {ok, Pid}.

await(Pid, Timeout) ->
    Return = case gen_event:call(Pid, ?MODULE, {await, self()}, Timeout) of
        Ref when is_reference(Ref) ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                      erlang:error(timeout)
            end;
        Result ->
            Result
    end,
    gen_event:stop(Pid),
    Return.

%% callbacks
init(#{}) ->
    {ok, #{idmap => #{}, resultmap => #{}, result => running}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({await, _From}, State=#{result := Result={ok, _}}) ->
    {ok, Result, State};
handle_call({await, _From}, State=#{result := Result={error, _}}) ->
    {ok, Result, State};
handle_call({await, From}, State=#{result := running}) ->
    Ref = make_ref(),
    {ok, Ref, State#{waiting => {From, Ref}}};
handle_call(_, State) ->
    {ok, ok, State}.

handle_event(Evt=#wrek_event{type={wrek, error}}, State) ->
    notify({error, Evt}, State);
handle_event(#wrek_event{type={wrek, done}}, State=#{resultmap := ResultMap}) ->
    notify({ok, ResultMap}, State#{resultmap => #{}});
handle_event(#wrek_event{type={vert, start},
                        id=Id,
                        msg=MsgTuple}, State=#{idmap := IdMap}) ->
    Name = element(1, MsgTuple),
    {ok, State#{idmap => IdMap#{Id => Name}}};
handle_event(#wrek_event{type={vert, done},
                         id=Id,
                         msg=Msg}, State=#{idmap := IdMap, resultmap := ResultMap}) ->
    Name = maps:get(Id, IdMap),
    {ok, #{result := Result}} = Msg,
    {ok, State#{resultmap => ResultMap#{Name => Result}}};
handle_event(_Evt, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_, _State) ->
    ok.

notify(Result, State) ->
    case maps:get(waiting, State, undefined) of
        undefined ->
            ok;
        {WaitingPid, Ref} ->
            WaitingPid ! {Ref, Result}
    end,
    {ok, maps:without([waiting], State#{result => Result})}.
