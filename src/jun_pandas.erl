%%
%% This module is a wrapper for some functions in pandas,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_pandas).

-export([max/2,
    min/2,
    count/2,
    median/2,
    sum/2]).

-export([read_csv/2,
    to_csv/1]).

%% Computations / Descriptive Stats

max(Pid, Axis) ->
    gen_server:call(Pid, {df_stats, max, Axis}).

min(Pid, Axis) ->
    gen_server:call(Pid, {df_stats, min, Axis}).

count(Pid, Axis) ->
    gen_server:call(Pid, {df_stats, count, Axis}).

median(Pid, Axis) ->
    gen_server:call(Pid, {df_stats, median, Axis}).

sum(Pid, Axis) ->
    gen_server:call(Pid, {df_stats, sum, Axis}).

%% Serialization / IO / Conversion

read_csv(Pid, Path) ->
    gen_server:call(Pid, {read_csv, Path}).

to_csv(Pid) ->
    gen_server:call(Pid, to_csv).
