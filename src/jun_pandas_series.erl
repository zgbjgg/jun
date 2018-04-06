%%
%% This module is a wrapper for some functions in pandas used in Series,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_pandas_series).

-export([combine/4]).

%% Binary operator functions

combine(Pid, Series, Series2, Keywords) ->
    gen_server:call(Pid, {'core.jun.series', [Series, combine, [Series2], 'None', Keywords]}, infinity).
