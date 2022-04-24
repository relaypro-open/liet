-module(gen_liet).
-behaviour(gen_server).

-export([start_link/1,
         start_link/2,
         start_link/3,
         get_state/2,
         stop/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DefaultApplyTimeout, 5000).
-record(state, {
          args    = #{},
          module,
          apply_timeout = ?DefaultApplyTimeout,
          liet
         }).

start_link(Module) ->
    start_link(Module, #{}).

start_link(Module, Opts) ->
    gen_server:start_link(?MODULE, #{module => Module,
                                     opts => Opts}, []).

start_link(Reg, Module, Opts) ->
    gen_server:start_link(Reg, ?MODULE, #{module => Module,
                                          opts => Opts}, []).

get_state(Ref, Timeout) ->
    gen_server:call(Ref, get_state, Timeout).

stop(Ref, Timeout) ->
    gen_server:call(Ref, stop, Timeout).

init(Args=#{module := Module, opts := Opts}) ->
    Targets = maps:get(targets, Opts, all),
    ApplyTimeout = maps:get(apply_timeout, Opts, ?DefaultApplyTimeout),
    case liet:apply(Module, Targets, ApplyTimeout) of
        {ok, LietState} ->
            {ok, #state{args    = Args,
                        module  = Module,
                        apply_timeout = ApplyTimeout,
                        liet    = LietState}};
        Error ->
            {stop, Error}
    end.

handle_call(get_state, _From, State=#state{liet=LietState}) ->
    {reply, LietState, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Cast, _State) ->
    erlang:error(function_clause).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{module=Module,
                                 liet=LietState,
                                 apply_timeout=ApplyTimeout}) ->
    case liet:destroy(Module, LietState, ApplyTimeout) of
        {ok, _LietState2} ->
            ok;
        Error ->
            erlang:error(Error)
    end.

code_change(_OldVsn, State, _Extras) ->
    {ok, State}.
