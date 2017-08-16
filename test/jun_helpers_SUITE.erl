-module(jun_helpers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_columns/1,
    test_jun_pandas_len_columns/1,
    test_jun_pandas_len_index/1,
    test_jun_pandas_memory_usage/1]).

all() ->
    [test_jun_pandas_columns,
     test_jun_pandas_len_columns,
     test_jun_pandas_len_index,
     test_jun_pandas_memory_usage].

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
    {ok, Columns} = jun_pandas:columns(Pid, DataFrame, []),
    Out = <<"name,age">>,
    ?assertEqual(Out, Columns).

test_jun_pandas_len_columns([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    {ok, LenColumns} = jun_pandas:len_columns(Pid, DataFrame, []),
    Out = 2,
    ?assertEqual(Out, LenColumns).

test_jun_pandas_len_index([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    {ok, LenIndex} = jun_pandas:len_index(Pid, DataFrame, []),
    Out = 6,
    ?assertEqual(Out, LenIndex).

test_jun_pandas_memory_usage([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    {ok, MemoryUsage} = jun_pandas:memory_usage(Pid, DataFrame, []),
    Out = <<"425.0+ bytes">>,
    ?assertEqual(Out, MemoryUsage).
