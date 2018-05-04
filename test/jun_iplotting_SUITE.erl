-module(jun_iplotting_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_iplot/1,
    test_jun_pandas_iplot_error/1]).

all() ->
    [test_jun_pandas_iplot,
     test_jun_pandas_iplot_error].

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

test_jun_pandas_iplot([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    Plot = jun_plotly:iplot(Pid, DataFrame, 'line/iplot', [{'kind', 'line'},
        {'x', 'name'}, {'y', 'age'}]),
    ?assertMatch({ok, _}, Plot).

test_jun_pandas_iplot_error([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    PlotError = jun_plotly:iplot(Pid, DataFrame, 'unknown/iplot', [{'kind', 'unknown'},
        {'x', 'name'}, {'y', 'unknown'}]),
    ?assertMatch({error, _}, PlotError).
