-module(jun_reindexing_selection_lbl_manipulation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, 'pandas.core.frame.DataFrame').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_drop/1,
    test_jun_pandas_rename/1,
    test_jun_pandas_append/1]).

all() ->
    [test_jun_pandas_drop,
     test_jun_pandas_rename,
     test_jun_pandas_append].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_atom(Cwd ++ "/../../lib/jun/test/files/csv.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_drop([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []), 
    {ok, {?DATAFRAME, DropDataFrame}} = jun_pandas:drop(Pid, DataFrame, 'age', [{'axis', 1}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, DropDataFrame),
    ?assertEqual({'pandas.core.frame.DataFrame', [<<"name">>], [[<<"Allison">>],
        [<<"George">>], [<<"Kristen">>], [<<"Debbie">>], [<<"Bjork">>],
        [<<"Katy">>]]}, Erl).

test_jun_pandas_rename([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, RenameDataFrame}} = jun_pandas:rename(Pid, DataFrame, '', [{'index', 'str'},
        {'columns', '{\'age\': \'agex\'}'}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, RenameDataFrame),
    ?assertEqual({'pandas.core.frame.DataFrame', [<<"name">>, <<"agex">>], [[<<"Allison">>,29],
        [<<"George">>,29], [<<"Kristen">>,30], [<<"Debbie">>,40], [<<"Bjork">>,40],
        [<<"Katy">>,30]]}, Erl).

test_jun_pandas_append([{jun_worker, Pid} | _]) ->
    {ok, {?DATAFRAME, DataFrameA}} = jun_pandas:read_string(Pid, "id,name\n1,Tutti", []),
    {ok, {?DATAFRAME, DataFrameB}} = jun_pandas:read_string(Pid, "id,name\n2,Ziggy", []),
    {ok, {?DATAFRAME, FinalDataFrame}} = jun_pandas:append(Pid, DataFrameA, DataFrameB, []),
    {ok, Erl} = jun_pandas:to_erl(Pid, FinalDataFrame),
    ?assertEqual({'pandas.core.frame.DataFrame', [<<"id">>, <<"name">>], [[1, <<"Tutti">>], [2, <<"Ziggy">>]]}, Erl).
