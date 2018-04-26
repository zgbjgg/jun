-module(jun_reshaping_sorting_transposing_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_sort_values/1,
    test_jun_pandas_sort_index/1]).

all() ->
    [test_jun_pandas_sort_values,
     test_jun_pandas_sort_index].

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

test_jun_pandas_sort_values([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path), 
    {ok, {?DATAFRAME, SortedDataFrame}} = jun_pandas:sort_values(Pid, DataFrame, 'None', [{'by', 'age'}, {'ascending', 'False'}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, SortedDataFrame),
    ?assertEqual({'pandas.core.frame.DataFrame', [<<"name">>,<<"age">>], [[<<"Debbie">>,40],
        [<<"Bjork">>,40], [<<"Kristen">>,30], [<<"Katy">>,30], [<<"Allison">>,29],
        [<<"George">>,29]]}, Erl).

test_jun_pandas_sort_index([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    {ok, {?DATAFRAME, SortedDataFrame}} = jun_pandas:sort_index(Pid, DataFrame, 'None', [{'ascending', 'False'}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, SortedDataFrame),
    ?assertEqual({'pandas.core.frame.DataFrame', [<<"name">>,<<"age">>], [[<<"Katy">>,30],
        [<<"Bjork">>,40], [<<"Debbie">>,40], [<<"Kristen">>,30], [<<"George">>,29],
        [<<"Allison">>,29]]}, Erl).
