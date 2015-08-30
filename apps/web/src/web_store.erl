-module(web_store).
-include_lib("cmon/include/logger.hrl").
-include_lib("web/include/db.hrl").

-export([init/3,handle/2,terminate/3]).

init(_Type, Req, _Opts) -> {ok, Req, _Opts}.

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

handle_upload(Req) ->
   Plist = multipart(Req, []),
   [Sid, Data, Mime, Type] = get_props(Plist, [sid,file,mime,type]),
   Uid = db_user:sid_to_uid(erlang:binary_to_integer(Sid)),
   FileId = user_file:store(Data, Uid, Type, Mime),
   db_msg:sys_notify(Uid, [<<"image/upload">>, FileId]),
   % cowboy_req:reply(200, Req). BUG?
   cowboy_req:reply(200, [{<<"connection">>, <<"close">>}], Req).

handle_get_file([#user_file{ id=FileId, mime=Type }], Req) ->
   Path = user_file:make_full_path(FileId),
   F = fun (Socket, Transport) -> Transport:sendfile(Socket, Path) end,
   Req2 = cowboy_req:set_resp_body_fun(F, Req),
   cowboy_req:reply(200, [{<<"content-type">>, Type}], Req2);
handle_get_file(A,_) -> ?ERR("handle_get_file() ~p", [A]).

handle_get_path(<<"avatar">>, Id, Req) ->
   handle_get_file(db_file:get(binary_to_integer(Id)), Req).

handle_get(Req) ->
   {[Type, Id], Req2} = cowboy_req:path_info(Req),
   handle_get_path(Type, Id, Req2).

handle_method(<<"POST">>, Req) -> handle_upload(Req);
handle_method(<<"GET">>, Req) -> handle_get(Req);
handle_method(Method, _Req) -> ?ERR("Unhandled method: ~p", [Method]).

handle(Req, State) ->
   {Method, Req2} = cowboy_req:method(Req),
   {ok, Req3} = handle_method(Method, Req2),
   {ok, Req3, State}.

terminate(_, _, _) -> ?INFO("TERM", []), ok.
