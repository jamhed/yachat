-module(web_image).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

-export([init/3,handle/2,terminate/3]).

init(_Type, Req, _Opts) ->
   ?INFO("IMAGE OPTS: ~p", [_Opts]),
   {ok, Req, _Opts}.

make_scale_file_name(FileId, SizeX, SizeY) ->
   string:join([ integer_to_list(X) || X <- [FileId, SizeX, SizeY] ], "-").

send_scaled_image(ScalePath, Type, Req) ->
   ?INFO("send_scaled: ~p ~p", [ScalePath, Type]),
   F = fun (Socket, Transport) -> Transport:sendfile(Socket, ScalePath) end,
   Req2 = cowboy_req:set_resp_body_fun(F, Req),
   cowboy_req:reply(200, [{<<"content-type">>, Type}], Req2).

scale_image(OriginPath, ScalePath, SizeX, SizeY) ->
   ?INFO("scale_image: ~p ~p", [OriginPath, ScalePath]),
   CvtRes = os:cmd(io_lib:format("convert ~s -resize ~px~p ~s", [OriginPath, SizeX, SizeY, ScalePath])),
   ?INFO("scale_image res: ~p", [CvtRes]).


handle_get_image([#user_file{ id=FileId, mime=Type }], SizeX, SizeY, Req, StorePath ) ->
   ScalePath = filename:join(StorePath, make_scale_file_name(FileId, SizeX, SizeY)),
   case filelib:is_file(ScalePath) of
      true -> ok;
      false ->
         OriginPath = filename:join(StorePath, integer_to_list(FileId)),
         scale_image(OriginPath, ScalePath, SizeX, SizeY)
   end,
   send_scaled_image(ScalePath, Type, Req);
handle_get_image(A,_,_,_,C) -> ?ERR("handle_get_image err: ~p ~p", [A,C]).

handle_get(Req, StorePath) ->
   {[Id, SizeX, SizeY], Req2} = cowboy_req:path_info(Req),
   handle_get_image( db_file:get(binary_to_integer(Id)), binary_to_integer(SizeX), binary_to_integer(SizeY), Req2, StorePath).


handle_method(<<"GET">>, Req, StorePath) -> handle_get(Req, StorePath);
handle_method(Method, _, _) -> ?ERR("Unhandled method: ~p", [Method]).

handle(Req, State = [StorePath]) ->
   ?INFO("web_image", []),
   {Method, Req2} = cowboy_req:method(Req),
   {ok, Req3} = handle_method(Method, Req2, StorePath),
   {ok, Req3, State}.

terminate(_, _, _) -> ?INFO("TERM", []), ok.
