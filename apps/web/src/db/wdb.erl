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



msg(Cid, UserId, Message) ->
	MsgId = dbd:next_id(message),
	dbd:put(#message{id=MsgId, conv_id=Cid, user_id=UserId, text=Message, stamp=now()}),
	MsgId.

query_user_pids(Uid) ->
	Q = qlc:q([ U#user_online.pid || U <- mnesia:table(user_online), U#user_online.user_id == Uid ]),
	dbd:do(Q).

query_user_convs(Uid) ->
	Q = qlc:q([ C#user_conv.conv_id || C <- mnesia:table(user_conv), C#user_conv.user_id == Uid ]),
	dbd:do(Q).

query_users_pids(UserIdList) -> [ query_user_pids(Uid) || Uid <- UserIdList ].

query_conv_pids(Cid) -> lists:flatten( query_users_pids( db_conv:users(Cid) ) ).

msg_notify(Cid, SenderId, Message, [ H | T]) ->
	H ! {new_msg, Cid, SenderId, Message},
	msg_notify(Cid, SenderId, Message, T);
msg_notify(_, _, _, []) -> ok.

conv_notify(Cid, SenderId, MsgId) ->
   {ok, #message{text=Text,stamp=Stamp}} = dbd:get(message, MsgId),
   msg_notify( Cid, SenderId, [cvt:now_to_time_binary(Stamp), Text], query_conv_pids(Cid) ).


