-module(wdb).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

metainfo() -> 
    #schema{name=dbd,tables=[
        #table{name=user,fields=record_info(fields,user), keys=[facebook_id, email]},
        #table{name=conv,fields=record_info(fields,conv), keys=[]},
        #table{name=user_conv, fields=record_info(fields, user_conv), keys=[user_id, conv_id]},
        #table{name=conv_msg, fields=record_info(fields, conv_msg), keys=[]},
        #table{name=user_msg, fields=record_info(fields, user_msg), keys=[]},
        #table{name=message, fields=record_info(fields, message), keys=[user_id]},
        #table{name=user_online, fields=record_info(fields, user_online), keys=[user_id, pid]}
    ]}.

user_to_proplist(#user{} = U) -> lists:zip(record_info(fields, user), tl(tuple_to_list(U))).

user_detail(Uid) ->
	case dbd:get(user, Uid) of
		{ok, #user{ id=Id, username=Name, email=Email }} -> [Id, Name, Email];
		_ -> []
	end.

users_detail(L) -> [ user_detail(Uid) || Uid <- L ].

query_conv(Id, Type) ->
	Q = qlc:q([C || C <- mnesia:table(conv), C#conv.id == Id, C#conv.type == Type]),
	case do(Q) of
		[C] -> {ok, C};
		_   -> {err, not_found}
	end.

list(Uid, Cid) ->
	Q = qlc:q([ Uin#user_conv.user_id ||
		U <- mnesia:table(user_conv), U#user_conv.conv_id == Cid, U#user_conv.user_id == Uid,
		Uin <- mnesia:table(user_conv), Uin#user_conv.conv_id == U#user_conv.conv_id
	]),
	do(Q).

query_conv_id(Uid1, Uid2) ->
	Q = qlc:q([ U1#user_conv.conv_id ||
			U1 <- mnesia:table(user_conv), U1#user_conv.user_id == Uid1,
			U2 <- mnesia:table(user_conv), U2#user_conv.user_id == Uid2, U1#user_conv.conv_id == U2#user_conv.conv_id
			]),
	do(Q).

is_user_in_conv(Uid, Cid) ->
	Q = qlc:q([ Id || #user_conv{ id=Id, user_id=_Uid, conv_id=_Cid } <- mnesia:table(user_conv), Uid == _Uid, Cid == _Cid ]),
	do(Q).


make_conv(Uid1, Uid2, Type) ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type=Type, stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid1, conv_id=Cid, stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid2, conv_id=Cid, stamp=now()}),
	Cid.

make_conv(Uid) ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type="generic", stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()}),
	Cid.

join_conv(Uid, Cid) ->
   case is_user_in_conv(Uid,Cid) of
      []  -> dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()});
      Err -> Err
   end.

leave_conv(Uid,Cid) -> [ dbd:delete(user_conv, Id) || Id <- is_user_in_conv(Uid,Cid) ].

find_conv(Uid1, Uid2, Type) ->
	case [ query_conv(Id,Type) || Id <- query_conv_id(Uid1, Uid2) ] of
		[{ok, C}]  -> {ok, C};
		_          -> {err, not_found}
	end.

msg(Cid, UserId, Message) ->
	MsgId = dbd:next_id(message),
	dbd:put(#message{id=MsgId, conv_id=Cid, user_id=UserId, text=Message, stamp=now()}),
	MsgId.

query_conv_users(Cid) ->
	Q = qlc:q([ C#user_conv.user_id || C <- mnesia:table(user_conv), C#user_conv.conv_id == Cid ]),
	do(Q).

query_user_pids(Uid) ->
	Q = qlc:q([ U#user_online.pid || U <- mnesia:table(user_online), U#user_online.user_id == Uid ]),
	do(Q).

query_user_convs(Uid) ->
	Q = qlc:q([ C#user_conv.conv_id || C <- mnesia:table(user_conv), C#user_conv.user_id == Uid ]),
	do(Q).


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

conv_history(Cid) -> conv_history(Cid, 10).

conv_history(Cid, Limit) ->
   Q = qlc:q([ M || M <- mnesia:table(message), M#message.conv_id == Cid ]),
   limit(Q, Limit).

query_users_pids(UserIdList) -> [ query_user_pids(Uid) || Uid <- UserIdList ].

query_conv_pids(Cid) -> lists:flatten( query_users_pids( query_conv_users(Cid) ) ).

msg_notify(Cid, SenderId, Message, [ H | T]) ->
	H ! {new_msg, Cid, SenderId, Message},
	msg_notify(Cid, SenderId, Message, T);
msg_notify(_, _, _, []) -> ok.

conv_notify(Cid, SenderId, MsgId) ->
   {ok, #message{text=Text,stamp=Stamp}} = dbd:get(message, MsgId),
   msg_notify( Cid, SenderId, [cvt:now_to_time_binary(Stamp), Text], query_conv_pids(Cid) ).

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
