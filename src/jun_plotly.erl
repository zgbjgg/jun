%%
%% This module is a wrapper for some functions in plotly using cufflinks,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_plotly).

-export([iplot/4]).

%% iPlotting

iplot(Pid, DataFrame, Filename, Keywords) ->
    % @FIXME: set the keyword as figure since this should be published in plotly
    gen_server:call(Pid, {'core.jun.dataframe.iplot', [DataFrame, Filename,
        [{'asFigure', true} | Keywords]]}, infinity).
