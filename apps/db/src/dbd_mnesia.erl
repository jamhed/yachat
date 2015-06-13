-module(dbd_mnesia).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("db/include/metainfo.hrl").
-compile(export_all).

start()    -> mnesia:start().
stop()     -> mnesia:stop().
destroy()  -> [mnesia:delete_table(list_to_atom(T))||{_,T}<-dir()], mnesia:delete_schema([node()]), ok.
version()  -> {version,"DBD MNESIA"}.
dir()      -> [{table,atom_to_list(T)}||T<-mnesia:system_info(local_tables)].
join()     -> mnesia:change_table_copy_type(schema, node(), disc_copies).
join(Node) ->
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    [{Tb, mnesia:add_table_copy(Tb, node(), Type)}
     || {Tb, [{N, Type}]} <- [{T, mnesia:table_info(T, where_to_commit)}
                               || T <- mnesia:system_info(tables)], Node==N].

% schema setup

create_table(Name,Options) -> mnesia:create_table(Name, Options).
add_table_index(Record, Field) -> mnesia:add_table_index(Record, Field).


make_table(T) ->
   create_table(T#table.name, [{attributes,T#table.fields},{T#table.copy_type, [node()]}]),
   [ add_table_index(T#table.name, Key) || Key <- T#table.keys ],
   T.

make_tables(Module) -> 
   IdT = #table{name=id_seq,fields=record_info(fields,id_seq),keys=[name]},
   make_table(IdT),
   [ make_table(T) || T <- (Module:metainfo())#schema.tables ].

tables(Modules) -> lists:flatten([ (M:metainfo())#schema.tables || M <- Modules ]).

initialize(Modules) ->
    ?INFO("initialize() ~p", [Modules]),
    mnesia:create_schema([node()]),
    [ make_tables(Module) || Module <- Modules ],
    mnesia:wait_for_tables([ T#table.name || T <- tables(Modules) ], 5000).

% queries

index(Tab,Index,Value) ->
    lists:flatten(many(fun() -> mnesia:index_read(Tab,Value,Index+1) end)).

get(RecordName, Key) -> just_one(fun() -> mnesia:read(RecordName, Key) end).
put(Records) when is_list(Records) -> void(fun() -> lists:foreach(fun mnesia:write/1, Records) end);
put(Record) -> put([Record]).
delete(Tab, Key) ->
    case mnesia:transaction(fun()-> mnesia:delete({Tab, Key}) end) of
        {aborted,Reason} -> {error,Reason};
        {atomic,_Result} -> ok end.
count(RecordName) -> mnesia:table_info(RecordName, size).
all(R) -> lists:flatten(many(fun() -> L= mnesia:all_keys(R), [ mnesia:read({R, G}) || G <- L ] end)).
next_id(RecordName, Incr) -> mnesia:dirty_update_counter({id_seq, RecordName}, Incr).
many(Fun) -> case mnesia:transaction(Fun) of {atomic, R} -> R; _ -> [] end.
void(Fun) -> case mnesia:transaction(Fun) of {atomic, ok} -> ok; {aborted, Error} -> {error, Error} end.
exec(Q) -> F = fun() -> qlc:e(Q) end, {atomic, Val} = mnesia:transaction(F), Val.

just_one(Fun) ->
   case mnesia:transaction(Fun) of
      {atomic, []}      -> [];
      {atomic, [R]}     -> [R];
      Error -> Error
   end.
