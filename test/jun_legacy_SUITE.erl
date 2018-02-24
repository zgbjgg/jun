-module(jun_legacy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').
-define(SERIES, 'pandas.core.frame.Series').
-define(LAMBDA, <<"lambda row : row['age'] + 7">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_legacy_query/1,
    test_jun_pandas_legacy_assignment/1]).

all() ->
    [test_jun_pandas_legacy_query,
     test_jun_pandas_legacy_assignment].

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

test_jun_pandas_legacy_query([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:legacy_query(Pid, DataFrame, 'age < 30', []),
    % the new dataframe just contains two values, check it!
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {'pandas.core.frame.DataFrame', [<<"name">>, <<"age">>],
        [[<<"Allison">>, 29], [<<"George">>, 29]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_legacy_assignment([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:legacy_query(Pid, DataFrame, 'age < 30', []),
    {ok, {?SERIES, Series}} = jun_pandas:apply(Pid, NewDataFrame, 'None', [{lambda, ?LAMBDA},
        {axis, 1}]),
    {ok, {?DATAFRAME, FinalDataFrame}} = jun_pandas:legacy_assignment(Pid, NewDataFrame,
        Series, [{column, 'age+7'}]),
    % the new dataframe just contains two values incremented by 7 as dictates lambda, check it!
    {ok, Erl} = jun_pandas:to_erl(Pid, FinalDataFrame),
    Out = {'pandas.core.frame.DataFrame', [<<"name">>, <<"age">>, <<"age+7">>],
        [[<<"Allison">>, 29, 36], [<<"George">>, 29, 36]]},
    ?assertEqual(Out, Erl).
