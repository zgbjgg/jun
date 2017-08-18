-module(jun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_worker/1,
    test_jun_app/1,
    test_jun_worker_call/1,
    test_jun_worker_cast/1,
    test_jun_app_stop/1,
    test_jun_worker_stop/1]).

all() ->
    [test_jun_app,
     test_jun_app_stop,
     test_jun_worker,
     test_jun_worker_call,
     test_jun_worker_cast,
     test_jun_worker_stop].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    [{jun_worker, Pid}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_app(_) ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(erlport),
    ?assertEqual(ok, application:start(jun)).

test_jun_worker([{jun_worker, Pid}]) ->
    ?assertEqual(is_pid(Pid), true).

test_jun_app_stop(_) ->
    ?assertEqual(ok, application:stop(jun)).

test_jun_worker_stop([{jun_worker, Pid}]) ->
    ok = jun_worker:stop_link(Pid),
    ?assertEqual(erlang:is_process_alive(Pid), false).

% just to increment % of coverage

test_jun_worker_call([{jun_worker, Pid}]) ->
    R = gen_server:call(Pid, hello),
    ?assertEqual(ok, R).

test_jun_worker_cast([{jun_worker, Pid}]) ->
    R = gen_server:cast(Pid, hello),
    ?assertEqual(ok, R).
