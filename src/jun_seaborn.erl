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
    jointplot/4,
    stripplot/4,
    swarmplot/4,
    boxplot/4,
    violinplot/4,
    lvplot/4,
    pointplot/4,
    barplot/4,
    countplot/4]).

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

%% Categorical plots

stripplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'stripplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

swarmplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'swarmplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

boxplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'boxplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

violinplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'violinplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

lvplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'lvplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

pointplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'pointplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

barplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'barplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

countplot(Pid, DataFrame, Filename, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.seaborn', [DataFrame, 'countplot',
        Filename, [{'data', DataFrame} | Keywords]]}, infinity).

%% @TODO:
%% * Distribution plots
%% * Regression plots
%% * Matrix plots
%% * Timeseries plots
