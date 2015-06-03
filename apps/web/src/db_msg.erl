-module(db_msg).
-compile({no_auto_import,[put/1,get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

get(MsgId) -> dbd:get(message, MsgId).

put(Cid, UserId, Message) ->
	MsgId = dbd:next_id(message),
	dbd:put(#message{id=MsgId, conv_id=Cid, user_id=UserId, text=Message, stamp=now()}),
	MsgId.

notify(_, _, _, []) -> ok;
notify(Cid, SenderId, Message, [ H | T]) ->
	H ! {new_msg, Cid, SenderId, Message},
	notify(Cid, SenderId, Message, T).

sys_notify(_, _, _, []) -> ok;
sys_notify(Cid, UserId, Message, [ H | T]) ->
	H ! {sys_msg, Cid, UserId, Message},
	sys_notify(Cid, UserId, Message, T).
