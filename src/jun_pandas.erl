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
    sum/4,
    unique/4]).

-export([read_csv/3,
    to_csv/3,
    to_html/3,
    to_json/3,
    to_datetime/3,
    read_sql/3,
    read_string/3,
    read_hdf/3,
    to_hdf/4]).

-export(['query'/4,
    head/4,
    tail/4]).

-export([to_erl/2]).

-export([columns/3,
    len_columns/3,
    len_index/3,
    memory_usage/3,
    info_columns/3,
    selection/4,
    single_selection/4]).

-export([plot/4]).

-export([groupby/4,
    'apply'/4]).

-export([sort_values/4,
    sort_index/4]).

-export([legacy_query/4,
    legacy_assignment/4]).

-export([drop/4,
    rename/4,
    append/4,
    update/4,
    set_index/4,
    reset_index/4,
    drop_duplicates/4]).

-export([fillna/4,
    dropna/4]).

-export([concat/4]).

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

unique(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, unique, [], Axis, Keywords]}, infinity).

%% Serialization / IO / Conversion

read_csv(Pid, Path, Keywords) ->
    gen_server:call(Pid, {'core.jun.pandas', [read_csv, [Path], Keywords]}, infinity).

to_csv(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_csv, [], 'None', Keywords]}, infinity).

to_html(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_html, [], 'None', Keywords]}, infinity).

to_json(Pid, DataFrame, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_json, [], 'None', Keywords]}, infinity).

to_datetime(Pid, Serie, Keywords) ->
    gen_server:call(Pid, {'core.jun.pandas', [to_datetime, [Serie], Keywords]}, infinity).

read_sql(Pid, Sql, Keywords) ->
    % as te original setup, we need data to create connection from py interface
    gen_server:call(Pid, {'core.jun.pandas', [read_sql, [Sql], Keywords]}, infinity).

read_string(Pid, String, Keywords) ->
    gen_server:call(Pid, {'core.jun', read_string, [String, Keywords]}, infinity).

read_hdf(Pid, Path, Keywords) ->
    gen_server:call(Pid, {'core.jun.pandas', [read_hdf, [Path], Keywords]}, infinity).

to_hdf(Pid, DataFrame, Path, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, to_hdf, [Path], 'None', Keywords]}, infinity).

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
selection(Pid, DataFrame, ColumnsStr, Keywords) when is_binary(ColumnsStr) ->
    selection(Pid, DataFrame, binary_to_list(ColumnsStr), Keywords);
selection(Pid, DataFrame, ColumnsStr, _Keywords) when is_list(ColumnsStr) ->
    ColumnsTokens = string:tokens(ColumnsStr, [$,]),
    Columns = list_to_tuple([ list_to_binary(C) || C <- ColumnsTokens]),
    gen_server:call(Pid, {'core.jun', selection, [DataFrame, Columns]}, infinity).

single_selection(Pid, DataFrame, Column, _Keywords) ->
    gen_server:call(Pid, {'core.jun', single_selection, [DataFrame, Column]}, infinity).

%% Plotting

plot(Pid, DataFrame, Save, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe.plot', [DataFrame, Save, Keywords]}, infinity).

%% Function application, GroupBy & Window

groupby(Pid, DataFrame, ColumnsStr, Keywords) when is_atom(ColumnsStr) ->
    groupby(Pid, DataFrame, atom_to_list(ColumnsStr), Keywords);
groupby(Pid, DataFrame, ColumnsStr, Keywords) when is_binary(ColumnsStr) ->
    groupby(Pid, DataFrame, binary_to_list(ColumnsStr), Keywords);
groupby(Pid, DataFrame, ColumnsStr, Keywords) when is_list(ColumnsStr) ->
    ColumnsTokens = string:tokens(ColumnsStr, [$,]),
    Columns = list_to_tuple([ list_to_binary(C) || C <- ColumnsTokens]),
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, groupby, [Columns], 'None', Keywords]}, infinity).

'apply'(Pid, DataFrame, Axis, Keywords) ->
    % lambda must come in keywords!
    Lambda = proplists:get_value(<<"lambda">>, Keywords, <<>>),
    Keywords0 = proplists:delete(<<"lambda">>, Keywords),
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, 'apply', [Lambda], Axis, Keywords0]}, infinity).

%% Reshaping, Sorting, Transposing

sort_values(Pid, DataFrame, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, sort_values, [], Axis, Keywords]}, infinity).

sort_index(Pid, DataFrame, _Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, sort_index, [], 'None', Keywords]}, infinity).

%% Legacy

legacy_query(Pid, DataFrame, Query, _Keywords) ->
    [Column, Operator, Value] = jun_parser:query(Query),
    gen_server:call(Pid, {'core.jun', legacy_query, [DataFrame, Column, Operator, Value]}, infinity).

legacy_assignment(Pid, DataFrame, Value, Keywords) ->
    % from keywords get the column to assign
    Column = begin
        ColumnAtom = proplists:get_value(<<"column">>, Keywords, <<"">>),
        case ColumnAtom of
            ColumAtom when is_atom(ColumAtom) ->
                ColumnList = atom_to_list(ColumnAtom),
                list_to_binary(ColumnList);
            ColumnAtom when is_binary(ColumnAtom) ->
                ColumnAtom
        end
    end,
    gen_server:call(Pid, {'core.jun', legacy_assignment, [DataFrame, Column, Value]}, infinity).

%% Reindexing, Selection & Label manipulation

drop(Pid, DataFrame, Column, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, drop, [Column], 'None', Keywords]}, infinity).

rename(Pid, DataFrame, _Column, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, rename, [], 'None', Keywords]}, infinity).

append(Pid, DataFrame, DataFrameToAppend, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, append, [DataFrameToAppend], 'None', Keywords]}, infinity).

update(Pid, DataFrame, DataFrameAsRow, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, update, [DataFrameAsRow], 'None', Keywords]}, infinity).

set_index(Pid, DataFrame, Column, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, set_index, [Column], 'None', Keywords]}, infinity).

reset_index(Pid, DataFrame, _Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, reset_index, [], 'None', Keywords]}, infinity).

drop_duplicates(Pid, DataFrame, Column, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, drop_duplicates, [Column], 'None', Keywords]}, infinity).

%% Missing data handling

fillna(Pid, DataFrame, _Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, fillna, [], 'None', Keywords]}, infinity).

dropna(Pid, DataFrame, _Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.dataframe', [DataFrame, dropna, [], 'None', Keywords]}, infinity).

%% Data manipulations

concat(Pid, DataFrame, DataFrameToConcat, Keywords) ->
    gen_server:call(Pid, {'core.jun.pandas', [concat, [[DataFrame, DataFrameToConcat]], Keywords]}, infinity).
