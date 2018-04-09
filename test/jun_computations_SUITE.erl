-module(jun_computations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_max/1,
    test_jun_pandas_min/1,
    test_jun_pandas_count/1,
    test_jun_pandas_median/1,
    test_jun_pandas_sum/1,
    test_jun_pandas_unique/1,
    test_jun_pandas_bad_axis/1]).

all() ->
    [test_jun_pandas_max,
     test_jun_pandas_min,
     test_jun_pandas_count,
     test_jun_pandas_median,
     test_jun_pandas_sum,
     test_jun_pandas_unique,
     test_jun_pandas_bad_axis].

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

test_jun_pandas_max([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Max = jun_pandas:max(Pid, DataFrame, age, []),
    ?assertEqual(Max, {ok, 40}).

test_jun_pandas_min([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Min = jun_pandas:min(Pid, DataFrame, age, []),
    ?assertEqual(Min, {ok, 29}).

test_jun_pandas_count([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Count = jun_pandas:count(Pid, DataFrame, age, []),
    ?assertEqual(Count, {ok, 6}).

test_jun_pandas_median([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Median = jun_pandas:median(Pid, DataFrame, age, []),
    ?assertEqual(Median, {ok, 30.0}).

test_jun_pandas_sum([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Sum = jun_pandas:sum(Pid, DataFrame, age, []),
    ?assertEqual(Sum, {ok, 198}).

test_jun_pandas_unique([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Unique = jun_pandas:unique(Pid, DataFrame, age, []),
    ?assertEqual(Unique, {ok, <<"29,30,40">>}).

test_jun_pandas_bad_axis([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Error = jun_pandas:max(Pid, DataFrame, unknown, []),
    ?assertEqual({error, {'exceptions.KeyError', "Atom('unknown')"}}, Error).
