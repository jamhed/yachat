-module(dbd).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("cmon/include/logger.hrl").
-include_lib("cmon/include/config.hrl").

-define(DBA, ?CFG(dbd)).

check_config() -> cfg:validate([?CFG_EXISTS(dbd), ?CFG_EXISTS(schema)]).

modules() -> ?CFG(schema).

initialize() ->
	DBA=?DBA,
	Schema = ?CFG(schema),
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

make_uid() -> erlang:phash2( [now(), make_ref()], 4294967296 ).

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

limit(QH, Limit) ->
	%% use a cursor to grab only Limit records
	F = fun() ->
		QC = qlc:cursor(qlc:sort(QH, {order, descending})),
		M = qlc:next_answers(QC, Limit),
		qlc:delete_cursor(QC),
		M
	end,
	{atomic, Msgs} = mnesia:transaction(F),
	Msgs.

limit(QH, 0, Limit) -> limit(QH, Limit);
limit(QH, 1, Limit) -> limit(QH, Limit);
limit(QH, Offset, Limit) ->
	?INFO("limit", []),
	F = fun() ->
		QC = qlc:cursor(qlc:sort(QH, {order, descending})),
		qlc:next_answers(QC, Offset - 1), % skip
		M = qlc:next_answers(QC, Limit),
		qlc:delete_cursor(QC),
		M
	end,
	{atomic, Msgs} = mnesia:transaction(F),
	Msgs.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.