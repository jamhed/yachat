-module(ws_main).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").
-export([init/4, stream/3, info/3, terminate/2]).
-record(state, {}).

route_msg([H|T], M,A) ->
   case H:msg(M,A) of
      skip  -> route_msg(T,M,A);
      Ret   -> Ret
   end.

init(_Transport, Req, Opts, _Active) ->
   ?INFO("init() pid:~p opts:~p active:~p ", [self(), Opts, _Active]),
   {ok, Req, #state{}}.

stream(JSON, Req, State) ->
   % ?INFO("RAW: ~p", [JSON]),
   case JSON of
      <<"ping">> -> {reply, JSON, Req, State};
      _ ->
         case jiffy:decode(JSON) of
            % numbered messages
            [ <<"nmsg">>, Seq, Msg, Args ] ->
               ?INFO("N-MSG: ~p MSG: ~p ARGS: ~p", [Seq, Msg, Args]),
               Raw = route_msg([ws_user, ws_msg, ws_conv, ws_stub], Msg, Args),
               Reply = jiffy:encode([<<"nmsg">>, Seq, Raw]),
               ?INFO("N-MSG REPLY: ~p", [Reply]),
               {reply, Reply, Req, State};
            % messages
            [ Msg | Args ] ->
               ?INFO("MSG: ~p ARGS: ~p", [Msg, Args]),
               Raw = route_msg([ws_user, ws_msg, ws_conv, ws_stub], Msg, Args),
               Reply = jiffy:encode(Raw),
               ?INFO("MSG REPLY: ~p", [Reply]),
               {reply, Reply, Req, State}
         end
   end.

info({msg, _Sender, Data}, Req, State) ->
   ?INFO("MSG: ~p", [Data]),
   {reply, Data, Req, State};

info({new_msg, ConvId, SenderId, Text}, Req, State) ->
   ?INFO("IN-MSG: ~p ~p ~p", [ConvId, SenderId, Text]),
   Reply = jiffy:encode([new_msg, ConvId, db_user:detail(SenderId), Text]),
   ?INFO("IN-MSG OUT: ~p", [Reply]),
   {reply, Reply, Req, State};

info({sys_msg, ConvId, Uid, Status}, Req, State) ->
   ?INFO("SYS-MSG: ~p ~p ~p", [ConvId, Uid, Status]),
   Reply = jiffy:encode([sys_msg, ConvId, db_user:detail(Uid), Status]),
   ?INFO("SYS OUT: ~p", [Reply]),
   {reply, Reply, Req, State}.

terminate(_Req, State) ->
   ?INFO("terminate: pid:~p state: ~p", [self(), State]),
   db_user:offline(self()),
   ok.
