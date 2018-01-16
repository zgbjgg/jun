%%
%% This module is a wrapper for some functions in seaborn,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_seaborn).

-export([lmplot/4,
    factorplot/4,
    pairplot/4,
    jointplot/4]).

%% Axis grids

lmplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'lmplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

factorplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'factorplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

pairplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'pairplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

jointplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'jointplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

%% @TODO:
%% * Categorical plots
%% * Distribution plots
%% * Regression plots
%% * Matrix plots
%% * Timeseries plots
