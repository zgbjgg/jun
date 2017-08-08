-module(jun_helpers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_columns/1]).

all() ->
    [test_jun_pandas_columns].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_atom(Cwd ++ "/../../lib/jun/test/files/csv.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_columns([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    {ok, Columns} = jun_pandas:columns(Pid, DataFrame),
    Out = <<"name,age">>,
    ?assertEqual(Out, Columns).
