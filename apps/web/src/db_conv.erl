-module(db_conv).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

get_by_users(Uid1, Uid2) ->
	Q = qlc:q([ U1#user_conv.conv_id ||
			U1 <- mnesia:table(user_conv), U1#user_conv.user_id == Uid1,
			U2 <- mnesia:table(user_conv), U2#user_conv.user_id == Uid2, U1#user_conv.conv_id == U2#user_conv.conv_id
			]),
	dbd:do(Q).

get_by_type(Id, Type) ->
	Q = qlc:q([C || C <- mnesia:table(conv), C#conv.id == Id, C#conv.type == Type]),
	case dbd:do(Q) of
		[C] -> {ok, C};
		_   -> {err, not_found}
	end.

find(Uid1, Uid2, Type) ->
	case [ get_by_type(Id,Type) || Id <- get_by_users(Uid1, Uid2) ] of
		[{ok, C}]  -> {ok, C};
		_          -> {err, not_found}
	end.

users(Cid) ->
	Q = qlc:q([ Uid || #user_conv{user_id=Uid, conv_id=_Cid} <- mnesia:table(user_conv), _Cid == Cid]),
	dbd:do(Q).

is_user_in(Uid, Cid) ->
	Q = qlc:q([ Id || #user_conv{ id=Id, user_id=_Uid, conv_id=_Cid } <- mnesia:table(user_conv), Uid == _Uid, Cid == _Cid ]),
	dbd:do(Q).

% make

p2p(Uid1, Uid2) ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type="p2p", stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid1, conv_id=Cid, stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid2, conv_id=Cid, stamp=now()}),
	Cid.

generic(Uid) ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type="generic", stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()}),
	Cid.

join(Uid, Cid) ->
   case is_user_in(Uid, Cid) of
      []  -> dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()});
      Err -> Err
   end.

leave(Uid, Cid) -> [ dbd:delete(user_conv, Id) || Id <- is_user_in(Uid, Cid) ].

history(Cid) -> history(Cid, 10).

history(Cid, Limit) ->
   Q = qlc:q([ M || M <- mnesia:table(message), M#message.conv_id == Cid ]),
   dbd:limit(Q, Limit).

pids(Cid) -> lists:flatten( db_user:pids( users(Cid) ) ).

notify(Cid, UserId, MsgId) ->
   {ok, #message{text=Text,stamp=Stamp}} = db_msg:get(MsgId),
   notify(Cid, UserId, Stamp, Text).

notify(Cid, UserId, Stamp, Text) ->
   db_msg:notify( Cid, UserId, [cvt:now_to_time_binary(Stamp), Text], db_conv:pids(Cid) ).

sys_notify(Cid, UserId, Text) ->
   db_msg:sys_notify( Cid, UserId, [cvt:now_to_time_binary(now()), Text], db_conv:pids(Cid) ).
