-module(web_upload).
-include_lib("cmon/include/logger.hrl").

-export([init/3,handle/2,terminate/3]).

init(_Type, Req, _Opts) ->
   ?INFO("OPTS: ~p", [_Opts]),
   {ok, Req, _Opts}.

handle_form({file, _Name, _File, _Type, _Enc}) -> file;
handle_form({data, <<"sid">>}) -> sid;
handle_form(_) -> skip.

handle_part({ok, Headers, Req}) -> 
   {ok, Data, Rest} = cowboy_req:part_body(Req),
   {Rest, [{handle_form(cow_multipart:form_data(Headers)), Data}]};
handle_part({done, Req}) -> {Req, []}.

multipart(Req, Prop) ->
   {Rest, Elem} = handle_part(cowboy_req:part(Req)),
   case Elem of 
      [] -> Prop;
      _ -> multipart(Rest, Elem ++ Prop)
   end.

write_user_file(Store, Link, Uid, Content) when is_number(Uid) ->
   FileName = integer_to_list(dbd:make_uid()),
   Path = filename:join(Store, FileName),
   file:write_file(Path, Content),
   ws_user:user_update(Uid, [<<"avatar">>, erlang:list_to_binary(filename:join(Link, FileName))]).

handle(Req, State = [StorePath, LinkPath]) ->
   Plist = multipart(Req, []),
   Sid = erlang:binary_to_integer(proplists:get_value(sid, Plist)),
   Content = proplists:get_value(file, Plist),
   Uid = db_user:sid_to_uid(Sid),
   ?INFO("PLIST: ~p ~p", [Sid,Uid]),
   write_user_file(StorePath, LinkPath, Uid, Content),
   {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
