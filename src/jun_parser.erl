% module to parse a query into a valid list fot legacy query input
% since df.query in built-in python pandas cannot work with columns with
% spaces or string values (maybe but using vars) so we build a single
% parser to decode a query and apply as a: df[df[COLUMN] OPERAND VALUE]
-module(jun_parser).

-export(['query'/1]).

-define(REGEX, "([<|>|<=|>=|==|!=])").

'query'(Query) when is_atom(Query) ->
    'query'(atom_to_list(Query));
'query'(Query) when is_binary(Query) ->
    'query'(binary_to_list(Query));
'query'(Query) when is_list(Query) ->
    Result = re:split(Query, ?REGEX, [{return, list}]),
    parse_legacy_query(Result).

% @hidden
% parse a query legacy splitted
parse_legacy_query([ColumnL, X, _, Y | ValueL]) ->
    parse_legacy_query([ColumnL, X ++ Y, lists:append(ValueL)]);
parse_legacy_query([ColumnL, OperandL, ValueL]) ->
    Column = list_to_binary(remove_whitespaces_fl(ColumnL)),
    Operand = list_to_binary(OperandL),
    Value = dtype(remove_whitespaces_fl(ValueL)),
    [Column, Operand, Value];
parse_legacy_query(_) ->
    [<<"_">>, <<"_">>, <<"_">>]. % by default all to none!

% @hidden
% remove whitespaces at first or last of column or value
remove_whitespaces_fl(S) ->
    NewS = case S of
        [32|S0] -> S0;
        _       -> S
    end,
    NewS0 = case lists:reverse(NewS) of
        [32|S1] -> S1;
        S2      -> S2
    end,
    lists:reverse(NewS0).

% @hidden
% try parsing value of comparison into a valid value: int, float or str
dtype(T) ->
    case catch list_to_integer(T) of
        {'EXIT', _} ->
            case catch list_to_float(T) of
                {'EXIT', _} -> list_to_binary(T);
                F           -> F
            end;
        I           -> I
    end.
