-module(jun_timedelta_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, <<"pandas.core.frame.DataFrame">>).
-define(SERIES, <<"pandas.core.frame.Series">>).
-define(LAMBDA, <<"lambda row : row['now'] - row['date']">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_timedelta_days/1]).

all() ->
    [test_jun_pandas_timedelta_days].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_binary(Cwd ++ "/../../lib/jun/test/files/csv_tdns.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_timedelta_days([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?SERIES, Series1}} = jun_pandas:single_selection(Pid, DataFrame, <<"date">>, []),
    {ok, {?SERIES, SeriesDt1}} = jun_pandas:to_datetime(Pid, Series1, []),
    {ok, {?SERIES, Series2}} = jun_pandas:single_selection(Pid, DataFrame, <<"now">>, []),
    {ok, {?SERIES, SeriesDt2}} = jun_pandas:to_datetime(Pid, Series2, []),
    {ok, {?DATAFRAME, DataFrame1}} = jun_pandas:legacy_assignment(Pid, DataFrame,
        SeriesDt1, [{<<"column">>, <<"date">>}]),
    {ok, {?DATAFRAME, DataFrame2}} = jun_pandas:legacy_assignment(Pid, DataFrame1,
        SeriesDt2, [{<<"column">>, <<"now">>}]),
    {ok, {?SERIES, SeriesApply}} = jun_pandas:apply(Pid, DataFrame2, <<"None">>, [{<<"lambda">>, ?LAMBDA},
        {<<"axis">>, 1}]),
    {ok, Series} = jun_pandas_timedelta:days(Pid, SeriesApply, <<"None">>, []),
    ?assertMatch({?SERIES, _}, Series).
