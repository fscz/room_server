-module(boss_db_adapter_tyrant).
-behaviour(boss_db_adapter).
-export([init/1, terminate/1, start/1, stop/0, find/2, find/7]).
-export([count/3, counter/2, incr/3, delete/2, save_record/2]).

-define(TRILLION, (1000 * 1000 * 1000 * 1000)).

start(_) ->
    ok.

stop() ->
    ok.

init(Options) ->
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 1978),
    DBConfigure = proplists:get_value(db_configure, Options, []),
    PrincipeOptions = [{hostname, Host}, {port, Port} | DBConfigure],
    principe:connect(PrincipeOptions).

terminate(Conn) ->
    gen_tcp:close(Conn).

find(Conn, Id) when is_list(Id) ->
    Type = infer_type_from_id(Id),
    case principe_table:get(Conn, list_to_binary(Id)) of
        Record when is_list(Record) ->
            case boss_record_lib:ensure_loaded(Type) of
                true -> activate_record(Record, Type);
                false -> {error, {module_not_loaded, Type}}
            end;
        {error, invalid_operation} ->
            undefined;
        {error, Reason} ->
            {error, Reason}
    end.

find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder) when is_atom(Type), is_list(Conditions),
                                                        is_integer(Max) orelse Max =:= all,
                                                        is_integer(Skip), is_atom(Sort), is_atom(SortOrder) ->
    case boss_record_lib:ensure_loaded(Type) of
        true ->
            AttributeTypes = boss_record_lib:attribute_types(Type),
            TypedSortOrder = case {SortOrder, proplists:get_value(Sort, AttributeTypes)} of
                {ascending, SortType} when SortType =:= float; SortType =:= integer; SortType =:= datetime; SortType =:= timestamp -> num_ascending;
                {descending, SortType} when SortType =:= float; SortType =:= integer; SortType =:= datetime; SortType =:= timestamp -> num_descending;
                {ascending, _} -> str_ascending;
                {descending, _} -> str_descending
            end,
            Query = build_query(Type, Conditions, Max, Skip, Sort, TypedSortOrder),
            ResultRows = principe_table:mget(Conn, principe_table:search(Conn, Query)),
            FilteredRows = case {Max, Skip} of
                {all, Skip} when Skip > 0 ->
                    lists:nthtail(Skip, ResultRows);
                _ ->
                    ResultRows
            end,
            lists:map(fun({_Id, Record}) -> activate_record(Record, Type) end, FilteredRows);
        false ->
            []
    end.

count(Conn, Type, Conditions) ->
    principe_table:searchcount(Conn, build_conditions(Type, Conditions)).

counter(Conn, Id) when is_list(Id) ->
    case principe_table:get(Conn, list_to_binary(Id)) of
        Record when is_list(Record) ->
            list_to_integer(binary_to_list(
                    proplists:get_value(<<"_num">>, Record, <<"0">>)));
        {error, _Reason} -> 0
    end.

incr(Conn, Id, Count) when is_list(Id) ->
    principe_table:addint(Conn, list_to_binary(Id), Count).

delete(Conn, Id) when is_list(Id) ->
    principe_table:out(Conn, list_to_binary(Id)).

save_record(Conn, Record) when is_tuple(Record) ->
    Type = element(1, Record),
    Id = case Record:id() of
        id ->
            atom_to_list(Type) ++ "-" ++ binary_to_list(principe_table:genuid(Conn));
        Defined when is_list(Defined) ->
            Defined
    end,
    RecordWithId = Record:set(id, Id),
    PackedRecord = pack_record(RecordWithId, Type),

    Result = principe_table:put(Conn, list_to_binary(Id), PackedRecord),
    case Result of
        ok -> {ok, RecordWithId};
        {error, Error} -> {error, [Error]}
    end.

% internal

pack_record(RecordWithId, Type) ->
    Columns = lists:map(fun
            (Attr) -> 
                Val = RecordWithId:Attr(),
                {attribute_to_colname(Attr), pack_value(Val)}
        end, RecordWithId:attribute_names()),
    [{attribute_to_colname('_type'), list_to_binary(atom_to_list(Type))}|Columns].

infer_type_from_id(Id) when is_list(Id) ->
    list_to_atom(hd(string:tokens(Id, "-"))).

activate_record(Record, Type) ->
    AttributeTypes = boss_record_lib:attribute_types(Type),
    apply(Type, new, lists:map(fun
                (Key) ->
                    Val = proplists:get_value(attribute_to_colname(Key), Record, <<"">>),
                    AttrType = proplists:get_value(Key, AttributeTypes, string),
                    case AttrType of
                        datetime -> unpack_datetime(Val);
                        timestamp -> DateTime = unpack_datetime(Val),
                            boss_record_lib:convert_value_to_type(DateTime, timestamp);
                        float -> list_to_integer(binary_to_list(Val)) / ?TRILLION;
                        _ -> boss_record_lib:convert_value_to_type(Val, AttrType)
                    end
            end, boss_record_lib:attribute_names(Type))).

attribute_to_colname(Attribute) ->
    list_to_binary(atom_to_list(Attribute)).

build_query(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    Query = build_conditions(Type, Conditions),
    Query1 = apply_limit(Query, Max, Skip),
    principe_table:query_order(Query1, atom_to_list(Sort), SortOrder).

apply_limit(Query, all, _) ->
    Query;
apply_limit(Query, Max, Skip) ->
    principe_table:query_limit(Query, Max, Skip).

build_conditions(Type, Conditions) ->
    build_conditions1([{'_type', 'equals', atom_to_list(Type)}|Conditions], []).

build_conditions1([], Acc) ->
    Acc;
build_conditions1([{Key, 'equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_eq, pack_value(Value)));
build_conditions1([{Key, 'not_equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_eq}, pack_value(Value)));
build_conditions1([{Key, 'in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_tokens(Value),
    build_conditions1(Rest, add_cond(Acc, Key, str_in_list, PackedValues));
build_conditions1([{Key, 'not_in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_tokens(Value),
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_in_list}, PackedValues));
build_conditions1([{Key, 'in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_tokens([Min, Max]),
    build_conditions1(Rest, add_cond(Acc, Key, num_between, PackedValues));
build_conditions1([{Key, 'not_in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_tokens([Min, Max]),
    build_conditions1(Rest, add_cond(Acc, Key, {no, num_between}, PackedValues));
build_conditions1([{Key, 'gt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_gt, pack_value(Value)));
build_conditions1([{Key, 'lt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_lt, pack_value(Value)));
build_conditions1([{Key, 'ge', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_ge, pack_value(Value)));
build_conditions1([{Key, 'le', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_le, pack_value(Value)));
build_conditions1([{Key, 'matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_regex, pack_value(Value)));
build_conditions1([{Key, 'not_matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_regex}, pack_value(Value)));
build_conditions1([{Key, 'contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_and, pack_value(Value)));
build_conditions1([{Key, 'not_contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_and}, pack_value(Value)));
build_conditions1([{Key, 'contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_and, pack_tokens(Values)));
build_conditions1([{Key, 'not_contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_and}, pack_tokens(Values)));
build_conditions1([{Key, 'contains_any', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_or, pack_tokens(Values)));
build_conditions1([{Key, 'contains_none', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_or}, pack_tokens(Values))).

add_cond(Acc, Key, Op, PackedVal) ->
    principe_table:query_add_condition(Acc, attribute_to_colname(Key), Op, [PackedVal]).

pack_tokens(Tokens) ->
    list_to_binary(string:join(lists:map(fun(V) -> binary_to_list(pack_value(V)) end, Tokens), " ")).

pack_datetime({Date, Time}) ->
    list_to_binary(integer_to_list(calendar:datetime_to_gregorian_seconds({Date, Time}))).

pack_now(Now) -> pack_datetime(calendar:now_to_datetime(Now)).

pack_value(V) when is_binary(V) ->
    V;
pack_value(V) when is_list(V) ->
    list_to_binary(V);
pack_value({MegaSec, Sec, MicroSec}) when is_integer(MegaSec) andalso is_integer(Sec) andalso is_integer(MicroSec) ->
    pack_now({MegaSec, Sec, MicroSec});
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    pack_datetime(Val);
pack_value(Val) when is_integer(Val) ->
    list_to_binary(integer_to_list(Val));
pack_value(Val) when is_float(Val) ->
    list_to_binary(integer_to_list(trunc(Val * ?TRILLION)));
pack_value(true) ->
    <<"1">>;
pack_value(false) ->
    <<"0">>.

unpack_datetime(<<"">>) -> calendar:gregorian_seconds_to_datetime(0);
unpack_datetime(Bin) -> calendar:universal_time_to_local_time(
        calendar:gregorian_seconds_to_datetime(list_to_integer(binary_to_list(Bin)))).
