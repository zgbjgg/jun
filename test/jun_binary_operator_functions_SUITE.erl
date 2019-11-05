-module(jun_binary_operator_functions_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').
-define(SERIES, 'pandas.core.frame.Series').

% define multiple lambdas for each phase
-define(LAMBDA_1, <<"lambda row : 'one' if row['age'] == 29 else 'none'">>).
-define(LAMBDA_2, <<"lambda row : 'two' if row['age'] == 30 else 'none'">>).
-define(LAMBDA_3, <<"lambda row : 'three' if row['age'] == 40 else 'none'">>).

% define the combine operation
-define(FUNC, <<"lambda a, b : b if b != 'none' else a">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_series_combine/1]).

all() ->
    [test_jun_pandas_series_combine].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_binary(Cwd ++ "/../../lib/jun/test/files/csv.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_series_combine([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?SERIES, Series1}} = jun_pandas:apply(Pid, DataFrame, <<"None">>, [{<<"lambda">>, ?LAMBDA_1},
        {<<"axis">>, 1}]),
    {ok, {?SERIES, Series2}} = jun_pandas:apply(Pid, DataFrame, <<"None">>, [{<<"lambda">>, ?LAMBDA_2},
        {<<"axis">>, 1}]),
    {ok, {?SERIES, Series3}} = jun_pandas:apply(Pid, DataFrame, <<"None">>, [{<<"lambda">>, ?LAMBDA_3},
        {<<"axis">>, 1}]),
    % combine the series1 and series2
    {ok, {?SERIES, SeriesC1}} = jun_pandas_series:combine(Pid, Series1, Series2, [{<<"func">>, ?FUNC}]),
    % combine the seriesc1 and series3
    {ok, {?SERIES, SeriesFinal}} = jun_pandas_series:combine(Pid, SeriesC1, Series3, [{<<"func">>, ?FUNC}]),
    % now add the series final to the df so we can compare in assert
    {ok, {?DATAFRAME, FinalDataFrame}} = jun_pandas:legacy_assignment(Pid, DataFrame,
        SeriesFinal, [{<<"column">>, <<"category">>}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, FinalDataFrame),
    Out = {'pandas.core.frame.DataFrame', [<<"name">>, <<"age">>, <<"category">>],
        [[<<"Allison">>, 29, <<"one">>], [<<"George">>, 29, <<"one">>],
         [<<"Kristen">>, 30, <<"two">>], [<<"Debbie">>, 40, <<"three">>],
         [<<"Bjork">>, 40, <<"three">>], [<<"Katy">>, 30, <<"two">>]]},
    ?assertEqual(Out, Erl).
