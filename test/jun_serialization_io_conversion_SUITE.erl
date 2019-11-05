-module(jun_serialization_io_conversion_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, <<"pandas.core.frame.DataFrame">>).
-define(SERIES, <<"pandas.core.frame.Series">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_read_csv/1,
    test_jun_pandas_to_csv/1,
    test_jun_pandas_to_html/1,
    test_jun_pandas_to_json/1,
    test_jun_pandas_to_erl/1,
    test_jun_pandas_bad_call/1,
    test_jun_pandas_to_datetime/1,
    test_jun_pandas_read_sql/1,
    test_jun_pandas_read_string/1]).

all() ->
    [test_jun_pandas_read_csv,
     test_jun_pandas_to_csv,
     test_jun_pandas_to_html,
     test_jun_pandas_to_json,
     test_jun_pandas_to_erl,
     test_jun_pandas_bad_call,
     test_jun_pandas_to_datetime,
     test_jun_pandas_read_sql,
     test_jun_pandas_read_string].

init_per_testcase(_, _Config) ->
    % for each case start a new worker
    {ok, Pid} = jun_worker:start_link(),
    % load the default file to execute tests
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_binary(Cwd ++ "/../../lib/jun/test/files/csv.txt"),
    [{jun_worker, Pid}, {path, Path}, {cwd, Cwd}].

end_per_testcase(_, _Config) ->
    % @todo stop the worker
    ok.

test_jun_pandas_read_csv([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, Opaque}} = jun_pandas:read_csv(Pid, Path, []),
    ?assertMatch({'$erlport.opaque', python, _}, Opaque).

test_jun_pandas_to_csv([{jun_worker, Pid}, {path, Path}, {cwd, Cwd}]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Csv} = jun_pandas:to_csv(Pid, DataFrame, []),
    {ok, Out} = file:read_file(Cwd ++ "/../../lib/jun/test/outputs/out.csv"),
    ?assertEqual(Out, Csv).

test_jun_pandas_to_html([{jun_worker, Pid}, {path, Path}, {cwd, Cwd}]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Html} = jun_pandas:to_html(Pid, DataFrame, []),
    {ok, Out} = file:read_file(Cwd ++ "/../../lib/jun/test/outputs/out.html"),
    ?assertEqual(binary_to_list(Out), Html).

test_jun_pandas_to_json([{jun_worker, Pid}, {path, Path}, {cwd, Cwd}]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Json} = jun_pandas:to_json(Pid, DataFrame, [{<<"orient">>, <<"records">>}]),
    {ok, Out} = file:read_file(Cwd ++ "/../../lib/jun/test/outputs/out.json"),
    ?assertEqual(Out, Json).

test_jun_pandas_to_erl([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, Erl} = jun_pandas:to_erl(Pid, DataFrame),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>],
        [[<<"Allison">>,29],[<<"George">>,29],[<<"Kristen">>,30],
         [<<"Debbie">>,40],[<<"Bjork">>,40],[<<"Katy">>,30]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_bad_call([{jun_worker, Pid}, _, {cwd, Cwd}]) ->
    Path = list_to_binary(Cwd ++ "/../../lib/jun/test/files/enoent.txt"),
    Error = jun_pandas:read_csv(Pid, Path, []),
    ?assertMatch({error, {'exceptions.IOError', _}}, Error).

test_jun_pandas_to_datetime([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?SERIES, Series}} = jun_pandas:single_selection(Pid, DataFrame, <<"age">>, []),
    {ok, SeriesDt} = jun_pandas:to_datetime(Pid, Series, []),
    ?assertMatch({?SERIES, _}, SeriesDt).

test_jun_pandas_read_sql([{jun_worker, Pid} | _]) ->
    {error,{Error, _}} = jun_pandas:read_sql(Pid, [<<"SELECT * FROM jun.table">>], [{<<"dsn">>, <<"DSN">>},
        {<<"username">>, <<"jun">>}, {<<"password">>, <<"jun">>}, {<<"database">>, <<"jun-database">>}]),
    ?assertEqual('pyodbc.InterfaceError', Error).

test_jun_pandas_read_string([{jun_worker, Pid}, _, {cwd, Cwd} | _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_string(Pid, <<"id,name\n1,Tutti">>, []),
    {ok, Csv} = jun_pandas:to_csv(Pid, DataFrame, []),
    {ok, Out} = file:read_file(Cwd ++ "/../../lib/jun/test/outputs/out.str"),
    ?assertEqual(Out, Csv).
