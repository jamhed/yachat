-module(ws_user).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

get_user_info([]) -> [fail, not_found, []];
get_user_info([#user{id=Uid}]) -> [ ok, db_user:detail(Uid) ];
get_user_info(Err) -> ?ERR("get_user_info(): ~p", [Err]), [fail, protocol].

% find user by email
user_email(Uid, Email) when is_number(Uid), is_binary(Email) -> get_user_info(db_user:get_by_email(Email));
user_email(_,_) -> [fail, protocol, sid].

% find user by id
user_info(SelfUid, QueryUid) when is_number(SelfUid), is_number(QueryUid) -> get_user_info(db_user:get(QueryUid));
user_info(_,_) -> [fail, protocol, sid].

% find users by ids
user_info_list(Uid, List) when is_number(Uid), is_list(List) -> db_user:get(List);
user_info_list(_,_) -> [fail, protocol, sid].

% get user
user_get(Uid) when is_number(Uid) ->
   [U] = db_user:get(Uid),
   Sid = db_user:online(Uid),
   [ok, User] = get_user_info([U]),
   [ok, Sid, User];
user_get(_) -> [fail, db, []].

% find user by facebook id
user_fb(FbId, ok) when is_binary(FbId) ->
   [U] = db_user:get_by_fb(FbId),
   Sid = db_user:online(U#user.id),
   [ok, User] = get_user_info([U]),
   [ok, Sid, User];
user_fb(_, _) -> [fail, protocol, sid].

% login user
user_login(Password, [#user{id=Uid, password=Password}]) ->
   db_user:offline(self()),
   Sid = db_user:online(Uid),
   [ok, User] = get_user_info(db_user:get(Uid)),
   [ok, Sid, User];
user_login(_, []) -> [fail, match];
user_login(_, _) -> [fail, protocol].
 
% create user and session
user_new(NewUid, ok) when is_number(NewUid) ->
   Sid = db_user:online(NewUid),
   [ok, Sid, db_user:detail_short(NewUid)];
user_new(Id, Status) -> ?ERR("user_new() id:~p status:~p", [Id, Status]), [fail].

check_keys(Uid, [#user{id=Uid}]) -> ne;
check_keys(_, []) -> ne; 
check_keys(_, _) -> e.

check_user_keys(U, Plist) ->
   E = proplists:get_value(<<"email">>, Plist),
   Fb = proplists:get_value(<<"facebook_id">>, Plist),
   Token = proplists:get_value(<<"facebook_token">>, Plist),
   [
      check_keys(U#user.id, db_user:get_by_email(E)),
      check_keys(U#user.id, db_user:get_by_fb(Fb)),
      ext_auth:check_fb(Fb, Token)
   ].

get_fb_avatar(Uid, FbId) when is_binary(FbId) ->
   {Mime, Data} = ext_auth:fb_picture(erlang:binary_to_integer(FbId)),
   FileId = user_file:store(Data, Uid, <<"avatar">>, Mime),
   db_msg:sys_notify(Uid, [<<"image/upload">>, FileId]);
get_fb_avatar(Uid, Fbid) ->
   ?INFO("no fb avatar:~p ~p", [Uid, Fbid]),
   ok.

% update user
user_update(Uid, Plist) when is_number(Uid), is_list(Plist) -> user_update(db_user:get(Uid), Plist);
user_update([User], Plist) when is_record(User, user) ->
   case check_user_keys(User, Plist) of
      [ne,ne,ok] ->
         Ux = db_user:set_by_props(User, Plist),
         dbd:put(Ux),
         get_fb_avatar(User#user.id, proplists:get_value(<<"facebook_id">>, Plist)),
         db_msg:sys_notify(User#user.id, [<<"user/update">>]),
         [ok];
      [_,_,Fb] ->
         [fail, exists, Fb]
   end;
user_update(_,_) -> [fail, args].

get_user_name(#user{id=Id, username=undefined}) -> erlang:integer_to_binary(Id);
get_user_name(#user{id=_Id, username=Name}) -> Name.

get_conv_name(Uid, [#conv{id=Cid, type="p2p"}]) ->
   ?INFO("~p ~p", [Uid, Cid]),
   case db_conv:peers(Cid, Uid) of
      [ PeerId | _] ->
         [Peer] = db_user:get(PeerId),
         [ {id,Cid}, {name, get_user_name(Peer)} ];
      [] -> [ {id,Cid}, {name, null} ]
   end;
get_conv_name(_Uid, [#conv{id=Cid}]) -> [ {id,Cid}, {name,null} ].

name_convs(_Uid, []) -> [];
name_convs(Uid, CidList) -> [ {get_conv_name(Uid, db_conv:get(Cid))} || Cid <- CidList].

user_conv_list(Uid) when is_number(Uid) ->
   Convs = db_user:conv(Uid),
   [ok, name_convs(Uid, Convs) ];
user_conv_list(_) -> [fail, protocol].

user_file_list(Uid) when is_number(Uid) ->
   Files = db_user:files(Uid),
   [ok, Files];
user_file_list(_) -> [fail, protocol].

user_file_list(Uid, Type) when is_number(Uid) ->
   Files = db_user:files(Uid, Type),
   [ok, Files];
user_file_list(_, _) -> [fail, protocol].

user_make_p2p(Uid, PeerId) ->
   case Uid of
      PeerId -> [fail, no_self_p2p];
      _ ->
         Cid = db_conv:p2p(Uid, PeerId),
         db_conv:notify(Uid, Cid, [<<"p2p">>, db_user:detail_short(Uid)]),
         [Peer | _] = db_user:detail_short(db_conv:peers(Cid, Uid)),
         [ok, Cid, Peer]
   end.

%
% MESSAGES
%

%msg check stored Sid 
msg(M = <<"user/get">>, [Uid]) ->
   [M] ++ user_get(Uid);

%msg login anonymously, create new user
msg(M = <<"user/new">>, []) ->
   NewUID = dbd:make_uid(),
   [M] ++ user_new(NewUID, dbd:put(#user{id=NewUID, stamp=now()}));

%msg login by email and password
msg(M = <<"user/login">>, [Email, Password]) ->
   [M] ++ user_login(Password, db_user:get_by_email(Email));

msg(M = <<"user/login/name">>, [Name, Password]) ->
   [M] ++ user_login(Password, db_user:get_by_name(Name));

%msg login by by facebook_id
msg(M = <<"user/fb">>, [FbId, Token]) ->
   [M] ++ user_fb(FbId, ext_auth:check_fb(FbId, Token));

%msg logout
msg(M = <<"user/logout">>, []) ->
   db_user:logout(self()),
   [M, ok];

%msg find user by email
msg(M = <<"user/email">>, [Uid, Email]) when is_number(Uid) ->
   ?INFO("~s sid:~p email:~p", [M, Uid, Email]),
   [M] ++ user_email(Uid, Email);

%msg get user profile 
msg(M = <<"user/profile">>, [Uid]) when is_number(Uid) ->
   [M] ++ get_user_info(db_user:get(Uid));

msg(M = <<"user/lookup">>, [_Uid, Term]) ->
   [M] ++ get_user_info(db_user:lookup(Term));

%msg get user info 
msg(M = <<"user/info">>, [Uid, PeerId]) when is_number(Uid), is_number(PeerId) ->
   [M] ++ user_info(Uid, PeerId);

%msg get users info [[UserId, Name, Email], ..., ]
msg(M = <<"user/info">>, [Uid, L]) when is_number(Uid), is_list(L) ->
   [M] ++ user_info_list(Uid, L);

%msg make p2p conv
msg(M = <<"user/p2p">>, [Uid, PeerId]) when is_number(Uid), is_number(PeerId) ->
   [M] ++ user_make_p2p(Uid, PeerId);

%msg update specified user profile field [Uid, Name1, Value1, ..., NameN, ValueN]
msg(M = <<"user/update">>, [Uid, {Plist}]) when is_number(Uid) ->
   [M] ++ user_update(Uid, Plist);

%msg get user's convs 
msg(M = <<"user/conv_list">>, [Uid]) when is_number(Uid) ->
   R = [M] ++ user_conv_list(Uid),
   ?INFO("R: ~p", [R]),
   R;

%msg get user's files
msg(M = <<"user/files">>, [Uid]) when is_number(Uid) ->
   [M] ++ user_file_list(Uid);

%msg get user's files
msg(M = <<"user/files">>, [Uid, _Type]) when is_number(Uid) ->
   [M] ++ user_file_list(Uid);

%msg delete file
msg(M = <<"user/file/delete">>, [Uid, FileId]) when is_number(Uid) ->
   [M, db_user:file_delete(Uid, FileId)];

%msg get user's online peers 
msg(M = <<"user/online">>, [Uid]) when is_number(Uid) ->
   [M, db_user:list_online(20)];

%msg get user's avatars 
msg(M = <<"user/avatar">>, [Uid]) when is_number(Uid) ->
   [M] ++ db_user:get_avatar(Uid);

%msg set user's avatar
msg(M = <<"user/avatar/set">>, [Uid, FileId]) when is_number(Uid), is_number(FileId) ->
   [M, db_user:set_avatar(Uid, FileId)];

msg(M = <<"user/add/friend">>, [Uid, FriendId]) when is_number(Uid), is_number(FriendId) ->
   ok = db_user:add_friend(Uid, FriendId),
   [M, ok];

msg(M = <<"user/del/friend">>, [Uid, FriendId]) when is_number(Uid), is_number(FriendId) ->
   ok = db_user:del_friend(Uid, FriendId),
   [M, ok];

msg(M = <<"user/get/friends">>, [Uid]) when is_number(Uid) ->
   [M] ++ [db_user:get_friends(Uid)];

% no match in this module
msg(_, _) -> skip.
