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
   ?INFO("RAW: ~p", [JSON]),
   case JSON of
      <<"ping">> -> {reply, JSON, Req, State};
      _ ->
         [ Msg | Args ] = jiffy:decode(JSON),
         ?INFO("JSON MSG: ~p ARGS: ~p", [Msg, Args]),
         Raw = route_msg([ws_user, ws_msg, ws_conv, ws_stub], Msg, Args),
         Reply = case Raw of <<"ping">> -> Raw; _ -> jiffy:encode(Raw) end,
         ?INFO("JSON REPLY: ~p", [Reply]),
         {reply, Reply, Req, State}
   end.

info({msg, _Sender, Data}, Req, State) ->
   ?INFO("MSG: ~p", [Data]),
   {reply, Data, Req, State};

info({new_msg, ConvId, SenderId, Text}, Req, State) ->
   ?INFO("TEXT MSG: ~p ~p ~p", [ConvId, SenderId, Text]),
   Reply = jiffy:encode([new_msg, ConvId, wdb:user_detail(SenderId), Text]),
   ?INFO("JSON MSG: ~p", [Reply]),
   {reply, Reply, Req, State}.

terminate(_Req, State) ->
   ?INFO("terminate: pid:~p state: ~p", [self(), State]),
   db_user:offline(self()),
   ok.
