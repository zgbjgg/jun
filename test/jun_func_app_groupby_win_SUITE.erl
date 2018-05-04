-module(jun_func_app_groupby_win_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').
-define(GROUPBY, 'pandas.core.groupby.DataFrameGroupBy').
-define(SERIES, 'pandas.core.frame.Series').
-define(LAMBDA, <<"lambda row : row['age'] + 7">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_groupby/1,
    test_jun_pandas_apply/1]).

all() ->
    [test_jun_pandas_groupby,
     test_jun_pandas_apply].

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

test_jun_pandas_groupby([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Group} = jun_pandas:groupby(Pid, DataFrame, 'name', []),
    ?assertMatch({?GROUPBY, {'$erlport.opaque', python, _}}, Group).

test_jun_pandas_apply([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Series} = jun_pandas:apply(Pid, DataFrame, 'None', [{lambda, ?LAMBDA},
        {axis, 1}]),
    ?assertMatch({?SERIES, {'$erlport.opaque', python, _}}, Series).
