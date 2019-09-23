-module(jun_missing_handling_data_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_fillna/1,
    test_jun_pandas_dropna/1]).

all() ->
    [test_jun_pandas_fillna,
     test_jun_pandas_dropna].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_atom(Cwd ++ "/../../lib/jun/test/files/csvm.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_fillna([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:fillna(Pid, DataFrame, '',
        [{value, '{\'age\': 0, \'name\': \'Unknown\'}'}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {'pandas.core.frame.DataFrame', [<<"name">>, <<"age">>],
        [[<<"Unknown">>, 10.0], [<<"George">>, 0.0], [<<"Allison">>, 36.0], [<<"Bjork">>, 40.0]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_dropna([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:dropna(Pid, DataFrame, '', []),
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {'pandas.core.frame.DataFrame', [<<"name">>, <<"age">>],
        [[<<"Allison">>, 36.0], [<<"Bjork">>, 40.0]]},
    ?assertEqual(Out, Erl).
