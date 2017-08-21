%%
%% This module is a wrapper for some functions in pandas,
%% using a single pid that holds a dataframe.
%% The pid is an instance of the jun_py_worker module and is capable
%% to execute commands over py interface, keeping the tracking of all transactions
%%
-module(jun_pandas).

-export([max/4,
    min/4,
    count/4,
    median/4,
    sum/4]).

-export([read_csv/2,
    to_csv/3,
    to_html/3,
    to_json/3]).

-export(['query'/4,
    head/4,
    tail/4]).

-export([to_erl/2]).

-export([columns/3,
    len_columns/3,
    len_index/3,
    memory_usage/3,
    info_columns/3,
    selection/4]).

-export([plot/4]).

%% DataFrames in erlang term
to_erl(Pid, {'$erlport.opaque', python, _} = OpaqueDataFrame) ->
    % tries convert to a erlang term, be careful of timeout in large dataframes!
    gen_server:call(Pid, {'core.jun', to_erl, [OpaqueDataFrame]}, infinity);
to_erl(_Pid, _) ->
    {error, no_opaque_dataframe}.

%% Computations / Descriptive Stats

max(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, max, [], Axis, Keywords]}, infinity).

min(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, min, [], Axis, Keywords]}, infinity).

count(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, count, [], Axis, Keywords]}, infinity).

median(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, median, [], Axis, Keywords]}, infinity).

sum(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, sum, [], Axis, Keywords]}, infinity).

%% Serialization / IO / Conversion

read_csv(Pid, Path) ->
    gen_server:call(Pid, {'pandas', read_csv, [Path]}, infinity).

to_csv(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_csv, [], 'None', Keywords]}, infinity).

to_html(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_html, [], 'None', Keywords]}, infinity).

to_json(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_json, [], 'None', Keywords]}, infinity).

%% Indexing / Iteration

'query'(Pid, DataFrame, Query, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, 'query', [Query], 'None', Keywords]}, infinity).

head(Pid, DataFrame, N, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, head, [N], 'None', Keywords]}, infinity).

tail(Pid, DataFrame, N, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, tail, [N], 'None', Keywords]}, infinity).

%% Helpers

columns(Pid, DataFrame, _Keywords) ->
    gen_server:call(Pid, {'core.jun', columns, [DataFrame]}, infinity).

len_columns(Pid, DataFrame, _Keywords) ->
    gen_server:call(Pid, {'core.jun', len_columns, [DataFrame]}, infinity).

len_index(Pid, DataFrame, _Keywords) ->
    gen_server:call(Pid, {'core.jun', len_index, [DataFrame]}, infinity).

memory_usage(Pid, DataFrame, _Keywords) ->
    gen_server:call(Pid, {'core.jun', memory_usage, [DataFrame]}, infinity).

info_columns(Pid, DataFrame, _Keywords) ->
    gen_server:call(Pid, {'core.jun', info_columns, [DataFrame]}, infinity).

selection(Pid, DataFrame, ColumnsStr, Keywords) when is_atom(ColumnsStr) ->
    selection(Pid, DataFrame, atom_to_list(ColumnsStr), Keywords);
selection(Pid, DataFrame, ColumnsStr, _Keywords) ->
    ColumnsTokens = string:tokens(ColumnsStr, [$,]),
    Columns = list_to_tuple([ list_to_binary(C) || C <- ColumnsTokens]),
    gen_server:call(Pid, {'core.jun', selection, [DataFrame, Columns]}, infinity).

%% Plotting

plot(Pid, DataFrame, Save, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.plot', [DataFrame, Save, Keywords]}, infinity).
