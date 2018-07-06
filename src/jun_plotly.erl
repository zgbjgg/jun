%%
%% This module is a wrapper for some functions in plotly using cufflinks,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_plotly).

-export([iplot/4,
    plot/4,
    extend/4]).

% since there are a low cases of plotly lib usage for now define separately
-define(JUN_IPLOT_DATAFRAME, jun_iplot_dataframe).
-define(JUN_IPLOT_PLOT, jun_iplot_plot).
-define(JUN_IPLOT_EXTEND, jun_iplot_extend).

%% iPlotting

iplot(Pid, DataFrame, Key, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.iplot', [?JUN_IPLOT_DATAFRAME, DataFrame, Key, Keywords]}, infinity).

plot(Pid, IPlot, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.iplot', [?JUN_IPLOT_PLOT, IPlot, Filename, Keywords]}, infinity).

extend(Pid, IPlotX, IPlotY, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.iplot', [?JUN_IPLOT_EXTEND, IPlotX, IPlotY, Keywords]}, infinity).
