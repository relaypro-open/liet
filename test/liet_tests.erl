-module(liet_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("liet/include/liet.hrl").

-define(Timeout, 2000).

liet_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(#{}) ->
             [
              %% Apply a graph and pass the State into the destroy
              ?_assertMatch({ok, _}, myenv:'#graph-'())
              , ?_assertMatch({ok, #{my_env_entry := my_key}}, liet:apply(myenv, ?Timeout))
              , ?_assertMatch({ok, #{my_env_entry := ok}}, liet:destroy(myenv, #{my_env_entry => my_key}, ?Timeout))

              % hierarchy test with trival destroy
              , ?_assertMatch({ok, #{
                               root_dir    := "/",
                               var_dir     := "/var",
                               tmp_dir     := "/tmp",
                               var_log_dir := "/var/log"
                              }},
                            liet:apply(linuxdirs, ?Timeout))
              , ?_assertMatch({ok, #{
                               root_dir    := ok,
                               var_dir     := ok,
                               tmp_dir     := ok,
                               var_log_dir := ok
                              }},
                            liet:destroy(linuxdirs, undefined, ?Timeout))

                %% Meck test with non-trivial destroy
              , ?_assertMatch({ok, #{string := ok}}, liet:apply(meckstring, ?Timeout))
              , ?_assertMatch(bar, string:is_empty("foo"))
              , ?_assertMatch({ok, #{string := ok}}, liet:destroy(meckstring, undefined, ?Timeout))
              , ?_assertMatch(false, string:is_empty("foo"))

                %% Apply specific target. Tests: (a) runs successfully and (b) does not apply the tmp_dir
              , ?_assertMatch({ok, _}, liet:apply(linuxdirs, [var_log_dir], ?Timeout))
              , ?_assertNotMatch({ok, #{tmp_dir := _}}, liet:apply(linuxdirs, [var_log_dir], ?Timeout))

                %% Ensure nodes are applied 1 time maximum
              , ?_assertMatch({ok, #{read := [{liet, 1}]}}, liet:apply(singlecounter, [read], ?Timeout))

                %% Ensure dep calculation is recursive
              , ?_assertMatch({ok, #{b := {ok, true}}}, liet:apply(recursivedeps, ?Timeout))
              , ?_assertMatch({ok, _}, liet:destroy(recursivedeps, undefined, ?Timeout))

                %% Use of gen_liet (Resource Acquisition Is Initialization)
              , ?_assertMatch({ok, _}, gen_liet:start_link({local, foodchain}, foodchain, #{apply_timeout => ?Timeout}))
              , ?_assertMatch(#{human := [think|_]}, gen_liet:get_state(foodchain, ?Timeout))
              , ?_assertNotMatch(#{sunlight := _}, gen_liet:get_state(foodchain, ?Timeout))
              , ?_assertMatch([_|_], ets:tab2list(foodchain))
              , ?_assertMatch(ok, gen_liet:stop(foodchain, ?Timeout))
              , ?_assertMatch(undefined, whereis(foodchain))
              , ?_assertMatch(undefined, ets:info(foodchain))
             ]
     end}.

setup() ->
    {ok, _} = application:ensure_all_started(meck),
    {ok, _} = application:ensure_all_started(liet),

    #{}.

teardown(#{}) ->
    ok.
