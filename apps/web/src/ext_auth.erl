-module(ext_auth).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").

check_fb(Fbid, Token) when is_binary(Fbid), is_binary(Token) ->
   Base = <<"https://graph.facebook.com/me?access_token=">>,
   {ok, {_Status, _Header, Body}} = httpc:request(binary_to_list(<<Base/binary, Token/binary>>)),
   {Props} = jiffy:decode(Body),
   case proplists:get_value(<<"id">>, Props) of
      Fbid -> ok;
      _   -> no_match
   end;
% proplist non-existant get_value
check_fb(undefined, undefined) -> ok;
check_fb(_, _) -> fb_fail.

fb_picture(Fbid) when is_integer(Fbid) ->
   Base = "https://graph.facebook.com/~p/picture?type=large&width=1024",
   Uri = io_lib:format(Base, [Fbid]),
   {ok, {_Status, Header, Body}} = httpc:request(Uri),
   Mime = proplists:get_value("content-type", Header),
   {Mime, Body}.
