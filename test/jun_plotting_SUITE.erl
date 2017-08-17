-module(jun_plotting_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_plot/1,
    test_jun_pandas_plot_error/1]).

all() ->
    [test_jun_pandas_plot,
     test_jun_pandas_plot_error].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(), 
    Path = list_to_atom(Cwd ++ "/../../lib/jun/test/files/csv_plot.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_plot([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    Plot = jun_pandas:plot(Pid, DataFrame, 'fig.png', [{'kind', 'line'},
        {'x', 'name'}, {'y', 'age'}]),
    ?assertEqual(Plot, {ok, <<"done">>}).

test_jun_pandas_plot_error([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, DataFrame} = jun_pandas:read_csv(Pid, Path),
    PlotError = jun_pandas:plot(Pid, DataFrame, 'fig.png', [{'kind', 'line'},
        {'x', 'name'}, {'y', 'unknown'}]),
    ?assertMatch({error, _}, PlotError).
