-module(ws_conv).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

% group list
msg(M = <<"conv/list">>, [UserId, ConvId]) ->
   List = db_conv:users(ConvId),
   Full = db_user:detail(List),
   [M, ok, Full];

msg(M = <<"conv/history">>, [Cid]) ->
   H = [ [db_user:detail(Uid), [cvt:now_to_time_binary(Stamp), Text]]
      || #message{stamp=Stamp,text=Text,user_id=Uid} <- db_conv:history(Cid) ],
   [M, ok, H];

% create group
msg(M = <<"conv/new">>, [null]) ->
   [M, fail, null_id];

msg(M = <<"conv/new">>, [UserId]) ->
   Cid = db_conv:generic(UserId),
   [M, ok, Cid];

% join group
msg(M = <<"conv/join">>, [UserId, ConvId]) ->
   db_conv:join(UserId, ConvId),
   [M, ok, ConvId];

% leave group
msg(M = <<"conv/leave">>, [UserId, ConvId]) ->
   db_conv:leave(UserId,ConvId),
   [M, ok, UserId, ConvId];

msg(_,_) -> skip.
