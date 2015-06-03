-module(db_user).
-compile({no_auto_import,[get/1]}).
-compile(export_all).
-include_lib("db/include/metainfo.hrl").
-include_lib("web/include/db.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("cmon/include/logger.hrl").
-define(SYSTEM,1).

to_proplist(#user{} = U) -> lists:zip(record_info(fields, user), tl(tuple_to_list(U))).

detail([H | T]) -> [detail(H)] ++ [ detail(U) || U <- T];
detail(Uid) ->
	case dbd:get(user, Uid) of
		{ok, #user{ id=Id, username=Name, email=Email }} -> [Id, Name, Email];
		_ -> []
	end.

% id of convs user is in
conv(Uid) ->
	Q = qlc:q([ C#user_conv.conv_id || C <- mnesia:table(user_conv), C#user_conv.user_id == Uid ]),
	dbd:do(Q).

pids([H | T]) -> [ pids(H) ] ++ [ pids(Uid) || Uid <- T ];
pids(Uid) ->
	Q = qlc:q([ U#user_online.pid || U <- mnesia:table(user_online), U#user_online.user_id == Uid ]),
	dbd:do(Q).


get(Id) -> dbd:get(user, Id).
get_by_fb(Id) -> dbd:index(user, facebook_id, Id).
get_by_email(Id) -> dbd:index(user, email, Id).

clear_online([]) -> ok;
clear_online([#user_online{id=Id} | T]) -> dbd:delete(user_online, Id), clear_online(T).

drop_online_status([]) -> ok;
drop_online_status([#user_online{id=Id, pid=Pid, user_id=Uid} | R]) ->
   ?INFO("drop_online_status: id=~p pid=~p", [Id, Pid]),
   notify_conv(Uid, <<"offline">>, conv(Uid)),
   dbd:delete(user_online, Id),
   drop_online_status(R).

offline(Pid) ->
   R = dbd:index(user_online, pid, Pid),
   drop_online_status(R).

notify_conv(_,_,[]) -> ok;
notify_conv(Uid, Text, [H | T]) ->
   db_conv:sys_notify(H, Uid, Text),
   notify_conv(Uid, Text, T).

online(#user{id=Uid}, Pid) ->
   case dbd:index(user_online, pid, Pid) of
      [] ->
         ?INFO("user_online: user_id=~p pid=~p", [Uid, Pid]),
         dbd:put(#user_online{id=dbd:next_id(user_online,1), stamp=now(), pid=Pid, user_id=Uid}),
         notify_conv(Uid, <<"online">>, conv(Uid));
      [_ | _] ->
         ?INFO("user_online: already online, skip: user_id=~p pid=~p", [Uid, Pid])
   end.
