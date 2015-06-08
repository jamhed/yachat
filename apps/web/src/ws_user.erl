-module(ws_user).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

%msg check user existance by id
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

%msg find user by facebook_id
msg(M = <<"user/fb">>, [FbId]) ->
   ?INFO("~s uid:~s", [M, FbId]),
   case db_user:get_by_fb(FbId) of
      [#user{id=Uid}]   ->
         [M, ok, db_user:detail(Uid)];
      []                ->
         [M, fail, not_found];
      Err               -> 
         ?ERR("~s err: ~p", [M, Err]),
         [M, fail, protocol] 
   end;

%msg find user by email
msg(M = <<"user/email">>, [Email]) ->
   ?INFO("~s email:~s", [M, Email]),
   case db_user:get_by_email(Email) of
      [#user{id=Uid}]   ->
         [M, ok, db_user:detail(Uid)];
      []                ->
         [M, fail];
      Err               -> ?ERR("~s err: ~p", [M, Err]),
         [M, fail, protocol]
   end;

%msg get user info [Id, Name, Email, FirstName, LastName, Gender, Avatar]
msg(M = <<"user/info">>, [Uid]) when is_number(Uid) ->
   [M, ok, db_user:detail(Uid)];

%msg get users info [[UserId, Name, Email], ..., ]
msg(M = <<"user/info">>, L) -> 
   [M, ok, db_user:detail(L)];

%msg create new user
msg(M = <<"user/new">>, []) ->
   ?INFO("~s new", [M]),
   NewUID = dbd:make_uid(),
   case dbd:put(U = #user{id=NewUID, stamp=now()}) of
      ok    ->
         db_user:online(U, self()),
         [M, new, NewUID];
      Err   -> ?ERR("~p err: ~p", [M, Err]),
         [M, fail, protocol]
   end;

%msg login by email and password
msg(M = <<"user/login">>, [Email, Password]) ->
   ?INFO("~s email:~s password:~s", [M, Email, Password]),
   case dbd:index(user, email, Email) of
      [#user{id=Uid, password=Password}]        ->
         db_user:offline(self()),
         db_user:online(#user{id=Uid}, self()),
         [M, ok, Uid];
      [] ->
         [M, fail, match];
      [_] ->
         [M, fail, match]; % password
      _ ->
         [M, fail, protocol]
   end;

%msg logout
msg(M = <<"user/logout">>, []) ->
   ?INFO("~s", [M]),
   db_user:offline(self()),
   [M, ok];

%msg update specified user profile field [Uid, Name1, Value1, ..., NameN, ValueN]
msg(M = <<"user/update">>, [Uid | List]) ->
   case db_user:get(Uid) of
      {ok, User}  ->
         Ux = db_user:set_by_name(User, List),
         dbd:put(Ux),
         [M, ok];
      _ ->
         [M, fail, no_user_uid]
   end;

%msg update profile information
msg(M = <<"user/register">>, [Uid, Email, Password, FirstName, LastName, UserName, Gender, Avatar]) when is_number(Uid) ->
   ?INFO("~p uid:~p email:~p username:~p", [M, Uid, Email, UserName]),
   case db_user:get(Uid) of
      {ok, User}  ->
         case dbd:index(user, email, Email) of
            []  ->
               dbd:put(User#user{
                  email=Email,
                  password=Password,
                  firstname=FirstName,
                  lastname=LastName,
                  username=UserName,
                  gender=Gender,
                  avatar=Avatar}),
               [M, ok, Uid];
            [#user{id=Uid}] ->
               dbd:put(User#user{
                  email=Email,
                  password=Password,
                  firstname=FirstName,
                  lastname=LastName,
                  username=UserName,
                  gender=Gender,
                  avatar=Avatar}),
               [M, ok, Uid];
            _   ->
               [M, fail, exists]
         end;
      _ ->
         [M, fail, no_user_id]
   end;

%msg update profile with facebook
msg(M = <<"user/facebook">>, [Uid, Id, Email, FirstName, LastName, UserName, Gender, Avatar]) when is_number(Uid) ->
   ?INFO("~s uid:~p fb_id:~p email:~p name:~p", [M, Uid, Id, Email, UserName]),
   case db_user:get(Uid) of
      {ok, User}  ->
         case db_user:get_by_fb(Id) of
            [] ->
               dbd:put(User#user{
                  email=Email,
                  firstname=FirstName,
                  lastname=LastName,
                  username=UserName,
                  gender=Gender,
                  facebook_id=Id,
                  avatar=Avatar}),
               [M, ok];
            [User] ->
               [M, fail, facebook_id_exists];
            Err ->
               ?INFO("~s err: ~p", [M, Err]),
               [M, fail, protocol]
         end;
      _ ->
         [M, fail, no_user_id]
   end;

%msg get user's convs 
msg(M = <<"user/conv_list">>, [Uid]) when is_number(Uid) ->
   ?INFO("~s uid:~p", [M, Uid]),
   Convs = db_user:conv(Uid),
   [M, ok, Convs];

% no match in this module
msg(_, _) -> skip.
