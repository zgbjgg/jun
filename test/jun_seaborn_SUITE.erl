-module(jun_seaborn_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_seaborn_lmplot/1,
    test_jun_seaborn_plot_error/1]).

all() ->
    [test_jun_seaborn_lmplot,
     test_jun_seaborn_plot_error].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_atom(Cwd ++ "/../../lib/jun/test/files/csv2.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_seaborn_lmplot([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    Plot = jun_seaborn:lmplot(Pid, DataFrame, 'fig.png', [{'x', 'month'},
        {'y', 'users'}, {'hue', 'smoker'}, {'fit_reg', 'False'}]),
    ?assertEqual(Plot, {ok, <<"seaborn.axisgrid.*">>}).

test_jun_seaborn_plot_error([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path),
    PlotError = jun_seaborn:lmplot(Pid, DataFrame, 'fig.png', [{'x', 'month'},
        {'y', 'unknown'}, {'hue', 'smoker'}]),
    ?assertEqual(PlotError, {ok, <<"error_format_data_frame_invalid">>}).
