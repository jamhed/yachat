-module(ws_user).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

% check user existance by id
msg(M = <<"user">>, [Uid]) when is_number(Uid) ->
   ?INFO("~s uid:~p", [M, Uid]),
   case db_user:get(Uid) of
      {ok, User} ->
         db_user:online(User, self()),
         [M, ok, User#user.id];
      Err ->
         ?INFO("~s uid: ~p fail: ~p", [M, Uid, Err]),
         [M, fail]
   end;

% check user existance by facebook id
msg(M = <<"user/fb">>, [FbId]) ->
   ?INFO("~s uid:~s", [M, FbId]),
   case db_user:get_by_fb(FbId) of
      {ok, User} ->
         [M, ok, User#user.id];
      _ ->
         [M, fail]
   end;

% check user email existance
msg(M = <<"user/email">>, [Email]) ->
   ?INFO("~s email:~s", [M, Email]),
   case db_user:get_by_email(Email) of
      {ok, #user{id=Uid}}              -> [M, ok, Uid];
      {error, not_found}               -> [M, fail];
      Err                              -> ?INFO("~s err: ~p", [M, Err]), [M, fail, protocol]
   end;

% [UserId, Name, Email]
msg(M = <<"user/info">>, [Uid]) when is_number(Uid) ->
   [M, ok, db_user:detail(Uid)];

msg(M = <<"user/info">>, L) -> 
   [M, ok, db_user:detail(L)];

% create new user
msg(M = <<"user/new">>, []) ->
   ?INFO("~s new", [M]),
   NewUID = dbd:make_uid(),
   case dbd:put(U = #user{id=NewUID, stamp=now()}) of
      ok    ->
         db_user:online(U, self()),
         [M, new, NewUID];
      Err   -> ?INFO("~s err: ~p", [M, Err]), [M, fail, protocol]
   end;

% login by email and password
msg(M = <<"user/login">>, [Email, Password]) ->
   ?INFO("~s email:~s password:~s", [M, Email, Password]),
   case dbd:index(user, email, Email) of
      [#user{id=Uid, password=Password}]        ->
         db_user:offline(self()),
         db_user:online(#user{id=Uid}, self()),
         [M, ok, Uid];
      []                                        -> [M, fail, match];
      [_]                                       -> [M, fail, match]; % password
      _                                         -> [M, fail, protocol]
   end;

msg(M = <<"user/logout">>, []) ->
   ?INFO("~s", [M]),
   db_user:offline(self()),
   [M, ok];

% update personal information
msg(M = <<"user/register">>, [Uid, Email, Password, Name, Gender]) when is_number(Uid) ->
   ?INFO("~p uid:~p email:~p name:~p", [M, Uid, Email, Name]),
   case db_user:get(Uid) of
      {ok, User}  ->
         case dbd:index(user, email, Email) of
            []  -> dbd:put(User#user{email=Email, password=Password, username=Name, sex=Gender}), [M, ok, Uid];
            _   -> [M, fail, exists]
         end;
      _ ->
         [M, fail, no_user_id]
   end;

% this comes from facebook auth
%  r.id, r.email, r.first_name, r.last_name, r.name, r.gender
msg(M = <<"user/facebook">>, [Uid, Id, Email, _, _, UserName, Gender]) when is_number(Uid) ->
   ?INFO("~s uid:~p fb_id:~p email:~p name:~p", [M, Uid, Id, Email, UserName]),
   case db_user:get(Uid) of
      {ok, User}  ->
         dbd:put(User#user{email=Email, username=UserName, sex=Gender, facebook_id=Id}),
         [M, ok];
      _ ->
         [M, fail, uid]
   end;

msg(M = <<"user/conv_list">>, [Uid]) when is_number(Uid) ->
   ?INFO("~s uid:~p", [M, Uid]),
   Convs = db_user:conv(Uid),
   [M, ok, Convs];

% no match in this module

msg(_, _) -> skip.
