%%
%% This module is a wrapper for some functions in pandas,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_pandas).

-export([max/3,
    min/3,
    count/3,
    median/3,
    sum/3]).

-export([read_csv/2]).

-export(['query'/3]).

-export([to_erl/2]).

%% DataFrames in erlang term
to_erl(Pid, {'$erlport.opaque', python, _} = OpaqueDataFrame) ->
    % tries convert to a erlang term, be careful of timeout in large dataframes!
    gen_server:call(Pid, {'core.jun', to_erl, [OpaqueDataFrame]}, 6000);
to_erl(_Pid, _) ->
    {error, no_opaque_dataframe}.

%% Computations / Descriptive Stats

max(Pid, DataFrame, Axis) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, max, [], Axis]}).

min(Pid, DataFrame, Axis) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, min, [], Axis]}).

count(Pid, DataFrame, Axis) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, count, [], Axis]}).

median(Pid, DataFrame, Axis) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, median, [], Axis]}).

sum(Pid, DataFrame, Axis) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, sum, [], Axis]}).

%% Serialization / IO / Conversion

read_csv(Pid, Path) ->
    gen_server:call(Pid, {'pandas', read_csv, [Path]}).

%% Indexing / Iteration

'query'(Pid, DataFrame, Query) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, 'query', [Query]]}).
