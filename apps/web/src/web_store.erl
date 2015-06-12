-module(web_store).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

-export([init/3,handle/2,terminate/3]).

init(_Type, Req, _Opts) ->
   ?INFO("OPTS: ~p", [_Opts]),
   {ok, Req, _Opts}.

handle_form({file, Name, _File, Type, _Enc}) -> {file, Type, Name};
handle_form({data, <<"sid">>}) -> sid;
handle_form(_) -> skip.

make_prop_elem({file, Type, Name}, Data) -> [{file, Data}, {mime, Type}, {type, Name}];
make_prop_elem(R, Data) when is_atom(R) -> [{R, Data}].

handle_part({ok, Headers, Req}) -> 
   {ok, Data, Rest} = cowboy_req:part_body(Req),
   { Rest, make_prop_elem( handle_form( cow_multipart:form_data(Headers) ), Data ) };
handle_part({done, Req}) -> {Req, []}.

multipart(Req, Prop) ->
   {Rest, Elem} = handle_part(cowboy_req:part(Req)),
   case Elem of 
      [] -> Prop;
      _ -> multipart(Rest, Elem ++ Prop)
   end.

get_props(Plist, Props) -> [ proplists:get_value(Prop, Plist) || Prop <- Props ].

write_user_file(Store, Data, Uid, Type, Mime) when is_number(Uid) ->
   FileId = dbd:make_uid(),
   Path = filename:join(Store, integer_to_list(FileId)),
   file:write_file(Path, Data),
   dbd:put(#user_file{ id=FileId, user_id=Uid, stamp=now(), mime=Mime, type=Type }).

handle_upload(Req, StorePath) ->
   Plist = multipart(Req, []),
   [Sid, Data, Mime, Type] = get_props(Plist, [sid,file,mime,type]),
   Uid = db_user:sid_to_uid(erlang:binary_to_integer(Sid)),
   write_user_file(StorePath, Data, Uid, Type, Mime),
   {ok, Req}.

handle_get_file([{FileId,Mime}], Req, StorePath ) when is_number(FileId) ->
   Path = filename:join(StorePath, integer_to_list(FileId)),
   F = fun (Socket, Transport) -> Transport:sendfile(Socket, Path) end,
   Req2 = cowboy_req:set_resp_body_fun(F, Req),
   Req2;

handle_get_file(A,_,C) -> ?ERR("handle_log_file() ~p ~p", [A,C]).

handle_get(Req, StorePath) ->
   {[Uid, Type], Req2} = cowboy_req:path_info(Req),
   handle_get_file( db_file:get( binary_to_integer(Uid), Type), Req2, StorePath).

handle_method(<<"POST">>, Req, StorePath) -> handle_upload(Req, StorePath);
handle_method(<<"GET">>, Req, StorePath) -> cowboy_req:reply(200, handle_get(Req, StorePath));
handle_method(Method, _, _) -> ?ERR("Unhandled method: ~p", [Method]).

handle(Req, State = [StorePath]) ->
   ?INFO("web_store", []),
   {Method, Req2} = cowboy_req:method(Req),
   {ok, Req3} = handle_method(Method, Req2, StorePath),
   {ok, Req3, State}.

terminate(_, _, _) -> ?INFO("TERM", []), ok.
