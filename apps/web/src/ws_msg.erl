-module(ws_msg).
-export([msg/2]).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

send_msg(Cid, UserId, Message) ->
   MsgId = db_msg:put(Cid, UserId, Message),
   db_conv:notify(Cid, UserId, MsgId),
   MsgId.

% p2p message, create conv if not exists
msg(M = <<"msg/p2p">>, [UserId, PeerId, Message]) ->
   case db_conv:find(UserId, PeerId, "p2p") of
      {ok, #conv{id=Cid}} ->
         MsgId = send_msg(Cid, UserId, Message),
         [M, ok, Cid, MsgId];
      _       ->
         Cid = db_conv:p2p(UserId, PeerId),
         MsgId = send_msg(Cid, UserId, Message),
         [M, ok, Cid, MsgId]
   end;

% conv message
msg(M = <<"msg/conv">>, [UserId, Cid, Message]) ->
   MsgId = send_msg(Cid, UserId, Message),
   [M, ok, Cid, MsgId];

msg(_,_) -> skip.

