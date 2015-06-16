-module(db_conv).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").

get(ConvId) -> dbd:get(conv, ConvId).

get_by_users(Uid1, Uid2) ->
	Q = qlc:q([ U1#user_conv.conv_id ||
			U1 <- mnesia:table(user_conv), U1#user_conv.user_id == Uid1,
			U2 <- mnesia:table(user_conv), U2#user_conv.user_id == Uid2, U1#user_conv.conv_id == U2#user_conv.conv_id
			]),
	dbd:do(Q).

get_by_type(Id, Type) ->
	Q = qlc:q([C || C <- mnesia:table(conv), C#conv.id == Id, C#conv.type == Type]),
	case dbd:do(Q) of
		[C] -> [C];
		_   -> []
	end.

find(Uid1, Uid2, Type) ->
	case [ get_by_type(Id,Type) || Id <- get_by_users(Uid1, Uid2) ] of
		[{ok, C}]  -> [C];
		_          -> []
	end.

peers(Cid, Uid) ->
	Q = qlc:q([ C#user_conv.user_id || C <- mnesia:table(user_conv),
      C#user_conv.conv_id == Cid, C#user_conv.user_id /= Uid]),
	dbd:do(Q).

users(Cid) ->
	Q = qlc:q([ Uid || #user_conv{user_id=Uid, conv_id=_Cid} <- mnesia:table(user_conv), _Cid == Cid]),
	dbd:do(Q).

% users except Uid (self)
users(Uid, Cid) -> lists:filter(fun(X) -> X /= Uid end, users(Cid)).

is_user_in(Uid, Cid) ->
	Q = qlc:q([ Id || #user_conv{ id=Id, user_id=_Uid, conv_id=_Cid } <- mnesia:table(user_conv), Uid == _Uid, Cid == _Cid ]),
	dbd:do(Q).

% make
p2p(Cid, Uid1, Uid2) when is_number(Cid), is_number(Uid1), is_number(Uid2) ->
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid1, conv_id=Cid, stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid2, conv_id=Cid, stamp=now()}),
   Cid.

new_p2p_conv() ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type="p2p", stamp=now()}),
	Cid.

get_p2p_cid(Uid1, Uid2) ->
   case find(Uid1, Uid2, "p2p") of
      [C] -> C#conv.id;
      _ ->
         case find(Uid2, Uid1, "p2p") of
            [C] -> C#conv.id;
            _ -> new_p2p_conv()
         end
   end.

p2p(Uid1, Uid2) -> p2p(get_p2p_cid(Uid1, Uid2), Uid1, Uid2).

generic(Uid) ->
	Cid = dbd:make_uid(),
	dbd:put(#conv{id=Cid, type="generic", stamp=now()}),
	dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()}),
	Cid.

join(Uid, Cid) ->
   case is_user_in(Uid, Cid) of
      []  ->
         dbd:put(#user_conv{id=dbd:next_id(user_conv), user_id=Uid, conv_id=Cid, stamp=now()}),
         notify(Uid, Cid, [<<"join">>, db_user:detail_short(Uid)]);
      Err -> Err
   end.

leave(Uid, Cid) ->
   notify(Uid, Cid, [<<"part">>, db_user:detail_short(Uid)] ),
   [ dbd:delete(user_conv, Id) || Id <- is_user_in(Uid, Cid) ].

history(Cid) -> history(Cid, 10).

history(Cid, Limit) ->
   Q = qlc:q([ M || M <- mnesia:table(message), M#message.conv_id == Cid ]),
   dbd:limit(Q, Limit).

pids(Uid, Cid) -> lists:flatten( db_user:pids( users(Uid, Cid) ) ).
pids(Cid) -> lists:flatten( db_user:pids( users(Cid) ) ).

notify(Cid, UserId, Stamp, Msg) ->
   db_msg:notify( Cid, UserId, [cvt:now_to_time_binary(Stamp), Msg], db_conv:pids(Cid) ).

notify(Uid, Cid, Msg) ->
   db_msg:conv_notify( Cid, [cvt:now_to_time_binary(now()), Msg], pids(Uid, Cid) ).

notify(Cid, Msg) ->
   db_msg:conv_notify( Cid, [cvt:now_to_time_binary(now()), Msg], pids(Cid) ).
