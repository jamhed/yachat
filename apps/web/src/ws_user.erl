-module(ws_user).
-compile(export_all).
% -export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

get_user_info([]) -> [fail, not_found];
get_user_info([#user{id=Uid}]) -> [ok, db_user:detail(Uid)];
get_user_info(Err) -> ?ERR("get_user_info(): ~p", Err), [fail, protocol].

% find user by facebook id
user_fb(Uid, FbId) when is_number(Uid), is_number(FbId) -> get_user_info(db_user:get_by_fb(FbId));
user_fb(_,_) -> [fail, protocol, sid].

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
   db_user:online(U, self()),
   get_user_info([U]);
user_get(_) -> [fail].

% create user and session
user_new(NewUid, ok) when is_number(NewUid) ->
   Sid = db_user:online(#user{id=NewUid}, self()),
   [ok, Sid, NewUid];
user_new(Id, Status) -> ?ERR("user_new() id:~p status:~p", [Id, Status]), [fail].

% login user
user_login(Password, [#user{id=Uid, password=Password}]) ->
   db_user:offline(self()),
   Sid = db_user:online(#user{id=Uid}, self()),
   [ok, Sid];
user_login(_, []) -> [fail, match];
user_login(_, _) -> [fail, protocol].
  
% update user
user_update(Uid, List) when is_number(Uid), is_list(List) -> user_update(db_user:get(Uid), List);
user_update([User], List) when is_record(User, user) ->
   Ux = db_user:set_by_name(User, List),
   dbd:put(Ux),
   [ok];
user_update(_,_) -> [fail].

user_conv_list(Uid) when is_number(Uid) ->
   Convs = db_user:conv(Uid),
   [ok, Convs];
user_conv_list(_) -> [fail, protocol].

%
% MESSAGES
%

msg(M = <<"user/get">>, [Uid]) ->
   ?INFO("~s uid: ~p", [M, Uid]),
   [M] ++ user_get(Uid);

%msg find user by facebook_id
msg(M = <<"user/fb">>, [Uid, FbId]) when is_number(Uid) ->
   ?INFO("~s uid:~p fbid:~p", [M, Uid, FbId]),
   [M] ++ user_fb(Uid, FbId);

%msg find user by email
msg(M = <<"user/email">>, [Uid, Email]) when is_number(Uid) ->
   ?INFO("~s sid:~p email:~p", [M, Uid, Email]),
   [M] ++ user_email(Uid, Email);

%msg get user info [Id, Name, Email, FirstName, LastName, Gender, Avatar]
msg(M = <<"user/info">>, [Uid, PeerId]) when is_number(Uid), is_number(PeerId) ->
   [M] ++ user_info(Uid, PeerId);

%msg get users info [[UserId, Name, Email], ..., ]
msg(M = <<"user/info">>, [Uid, L]) when is_number(Uid), is_list(L) ->
   [M] ++ user_info_list(Uid, L);

%msg create new user
msg(M = <<"user/new">>, []) ->
   ?INFO("~s new", [M]),
   NewUID = dbd:make_uid(),
   [M] ++ user_new(NewUID, dbd:put(#user{id=NewUID, stamp=now()}));

%msg login by email and password
msg(M = <<"user/login">>, [Email, Password]) ->
   ?INFO("~s email:~s password:~s", [M, Email, Password]),
   [M] ++ user_login(Password, dbd:index(user, email, Email));

%msg logout
msg(M = <<"user/logout">>, []) ->
   ?INFO("~s", [M]),
   db_user:offline(self()),
   [M, ok];

%msg update specified user profile field [Uid, Name1, Value1, ..., NameN, ValueN]
msg(M = <<"user/update">>, [Uid | List]) when is_number(Uid) ->
   ?INFO("~s uid:~p List:~p", [M, Uid, List]),
   [M] ++ user_update(Uid, List);

%msg get user's convs 
msg(M = <<"user/conv_list">>, [Uid]) when is_number(Uid) ->
   ?INFO("~s uid:~p", [M, Uid]),
   [M] ++ user_conv_list(Uid);

% no match in this module
msg(_, _) -> skip.
