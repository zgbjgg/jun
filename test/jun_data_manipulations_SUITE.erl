-module(jun_data_manipulations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, <<"pandas.core.frame.DataFrame">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_concat/1]).

all() ->
    [test_jun_pandas_concat].

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

test_jun_pandas_concat([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrameSource}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, DataFrameToConcat}} = jun_pandas:read_string(Pid,
      <<"name,age\nJulian,40">>, []),
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:concat(Pid, DataFrameSource, DataFrameToConcat,
      [{<<"ignore_index">>, <<"True">>}, {<<"sort">>, <<"False">>}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, DataFrame),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>],
        [[<<"Allison">>,29],[<<"George">>,29],[<<"Kristen">>,30],
         [<<"Debbie">>,40],[<<"Bjork">>,40],[<<"Katy">>,30],[<<"Julian">>,40]]},
    ?assertEqual(Out, Erl).
