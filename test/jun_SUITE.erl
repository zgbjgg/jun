-module(jun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_worker/1,
    test_jun_app/1]).

all() ->
    [test_jun_app,
     test_jun_worker].

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
