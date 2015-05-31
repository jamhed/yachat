-module(dbd).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("cmon/include/logger.hrl").

-define(DBA, cfg:get_a(db, dbd)).

% api

check_config() ->
   DBD = cfg:get_a(db, dbd, ""),
   SCHEMA = cfg:get_a(db, schema, []),
   case DBD of
      "" -> erlang:error(dbd_config_no_driver);
      _  -> ok
   end,
   case SCHEMA of
      [] -> erlang:error(dbd_config_no_schema);
      _  -> ok
   end,
   {DBD, SCHEMA}.


modules() -> cfg:get(schema).

initialize()            ->
   DBA=?DBA,
   Schema = cfg:get(schema),
   join(),
   DBA:initialize(Schema).

start()                 -> DBA=?DBA, DBA:start().
stop()                  -> DBA=?DBA, DBA:stop().

destroy()               -> DBA=?DBA, DBA:destroy().
join()                  -> DBA=?DBA, DBA:join().
join(Node)              -> DBA=?DBA, DBA:join(Node).
dir()                   -> DBA=?DBA, DBA:dir().
tables()                -> DBA=?DBA, DBA:tables(modules()).

delete(Tab, Key)        -> DBA=?DBA, DBA:delete(Tab, Key).
count(Name)             -> DBA=?DBA, DBA:count(Name).
all(Name)               -> DBA=?DBA, DBA:all(Name).
next_id(Name, Incr)     -> DBA=?DBA, DBA:next_id(Name, Incr).
next_id(Name)           -> DBA=?DBA, DBA:next_id(Name, 1).

index(Name, Key, Value) ->
   DBA=?DBA,
   Table = table(Name),
   Index = string:str(Table#table.fields, [Key]),
   DBA:index(Name, Index, Value).

table(Name) -> lists:keyfind(Name,#table.name, tables()).

% low-level primitives

put(Record) ->
    DBA=?DBA,
    DBA:put(Record).

get(Name, Key) ->
    DBA=?DBA,
    DBA:get(Name, Key).

get(Name, Key, Default) ->
    DBA=?DBA,
    case DBA:get(Name, Key) of
        {ok,{Name,Key,Value}} ->
            ?INFO("get() name:~p key:~p value:~p", [Name, Key, Value]),
            {ok,Value};
        {error, _B} ->
            ?INFO("get() name:~p key:~p default:~p", [Name, Key, Default]),
            DBA:put({Name,Key,Default}),
            {ok,Default} end.
