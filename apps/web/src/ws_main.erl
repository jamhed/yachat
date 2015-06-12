-module(ws_main).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-export([init/4, stream/3, info/3, terminate/2]).
-record(state, {}).

handlers() -> [ws_user, ws_msg, ws_conv, ws_stub].

route_msg([H|T], M,A) ->
   case H:msg(M,A) of
      skip  -> route_msg(T,M,A);
      Ret   -> Ret
   end.

module_route(Uid, Msg, Args) -> module_route(Msg, [Uid] ++ Args).
module_route(Msg, Args) -> route_msg(handlers(), Msg, Args).

pre_handle(Req, State, [<<"nmsg">>, Seq, [Msg, Sid | Args]]) when is_number(Seq), is_number(Sid)  ->
   ?INFO("N-MSG: ~p MSG: ~p SID: ~p ARGS: ~p", [Seq, Msg, Sid, Args]),
   Raw = module_route( db_user:sid_to_uid(Sid), Msg, Args ),
   Reply = jiffy:encode([<<"nmsg">>, Seq, Raw]),
   ?INFO("N-MSG REPLY: ~p", [Reply]),
   {reply, Reply, Req, State};

pre_handle(Req, State, [ Msg, Sid | Args ]) when is_number(Sid), is_list(Args) ->
   ?INFO("A-MSG: ~p SID: ~p ARGS: ~p", [Msg, Sid, Args]),
   Raw =  module_route( db_user:sid_to_uid(Sid), Msg, Args ),
   Reply = jiffy:encode(Raw),
   ?INFO("A-MSG REPLY: ~p", [Reply]),
   {reply, Reply, Req, State};

pre_handle(Req, State, [ Msg, Args ]) when is_list(Args) ->
   ?INFO("MSG: ~p ARGS: ~p", [Msg, Args]),
   Raw = module_route( Msg, Args ),
   Reply = jiffy:encode(Raw),
   ?INFO("MSG REPLY: ~p", [Reply]),
   {reply, Reply, Req, State};

pre_handle(Req, State, _) -> {reply, <<"income_message_format_error">>, Req, State}.

init(_Transport, Req, Opts, _Active) ->
   ?INFO("init() pid:~p opts:~p active:~p ", [self(), Opts, _Active]),
   {ok, Req, #state{}}.

stream(M = <<"ping">>, Req, State) -> {reply, M, Req, State};

stream(JSON, Req, State) ->
   % ?INFO("RAW: ~p", [Raw]),
   pre_handle(Req, State, jiffy:decode(JSON)).

info({msg, _Sender, Data}, Req, State) ->
   ?INFO("MSG: ~p", [Data]),
   {reply, Data, Req, State};

info({new_msg, ConvId, SenderId, Text}, Req, State) ->
   ?INFO("IN-MSG: ~p ~p ~p", [ConvId, SenderId, Text]),
   Reply = jiffy:encode([new_msg, ConvId, db_user:detail_short(SenderId), Text]),
   ?INFO("IN-MSG OUT: ~p", [Reply]),
   {reply, Reply, Req, State};

info({sys_msg, Status}, Req, State) ->
   ?INFO("SYS-MSG: ~p", [Status]),
   Reply = jiffy:encode([sys_msg, Status]),
   ?INFO("SYS OUT: ~p", [Reply]),
   {reply, Reply, Req, State};

info({conv_msg, ConvId, Status}, Req, State) ->
   ?INFO("CONV-MSG: ~p ~p", [ConvId, Status]),
   Reply = jiffy:encode([conv_msg, ConvId, Status]),
   ?INFO("CONV OUT: ~p", [Reply]),
   {reply, Reply, Req, State}.

terminate(_Req, State) ->
   ?INFO("terminate: pid:~p state: ~p", [self(), State]),
   db_user:offline(self()),
   ok.
