-module(ext_auth).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").

check_fb(Uid, Token) when is_binary(Uid), is_binary(Token) ->
   Base = <<"https://graph.facebook.com/me?access_token=">>,
   {ok, {_Status, _Header, Body}} = httpc:request(binary_to_list(<<Base/binary, Token/binary>>)),
   {Props} = jiffy:decode(Body),
   case proplists:get_value(<<"id">>, Props) of
      Uid -> ok;
      _   -> no_match
   end;
% proplist non-existant get_value
check_fb(undefined, undefined) -> ok;
check_fb(_, _) -> fb_fail.
