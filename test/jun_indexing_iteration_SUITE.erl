-module(jun_indexing_iteration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATAFRAME, <<"pandas.core.frame.DataFrame">>).
-define(SERIES, <<"pandas.core.frame.Series">>).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_jun_pandas_query/1,
    test_jun_pandas_head/1,
    test_jun_pandas_tail/1,
    test_jun_pandas_mask/1]).

all() ->
    [test_jun_pandas_query,
     test_jun_pandas_head,
     test_jun_pandas_tail,
     test_jun_pandas_mask].

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

test_jun_pandas_query([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:query(Pid, DataFrame, <<"age < 30">>, []),
    % the new dataframe just contains two values, check it!
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>],
        [[<<"Allison">>, 29], [<<"George">>, 29]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_head([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:head(Pid, DataFrame, 1, []),
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>],
        [[<<"Allison">>, 29]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_tail([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?DATAFRAME, NewDataFrame}} = jun_pandas:tail(Pid, DataFrame, 1, []),
    {ok, Erl} = jun_pandas:to_erl(Pid, NewDataFrame),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>],
        [[<<"Katy">>, 30]]},
    ?assertEqual(Out, Erl).

test_jun_pandas_mask([{jun_worker, Pid}, {path, Path}, _]) ->
    {ok, {?DATAFRAME, DataFrame}} = jun_pandas:read_csv(Pid, Path, []),
    {ok, {?SERIES, Series1}} = jun_pandas:mask(Pid, DataFrame, <<"name">>, [{<<"cond">>, <<"pd.isnull">>},
      {<<"other">>, <<"df['age']">>}]),
    {ok, {?DATAFRAME, DataFrame1}} = jun_pandas:legacy_assignment(Pid, DataFrame,
        Series1, [{<<"column">>, <<"masked">>}]),
    {ok, Erl} = jun_pandas:to_erl(Pid, DataFrame1),
    Out = {?DATAFRAME, [<<"name">>, <<"age">>, <<"masked">>],
        [[<<"Allison">>, 29, <<"Allison">>], [<<"George">>, 29, <<"George">>],
         [<<"Kristen">>, 30, <<"Kristen">>],
         [<<"Debbie">>, 40, <<"Debbie">>], [<<"Bjork">>, 40, <<"Bjork">>],
         [<<"Katy">>, 30, <<"Katy">>]]},
    ?assertEqual(Out, Erl). 
