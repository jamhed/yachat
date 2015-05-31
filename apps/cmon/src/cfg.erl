-module(cfg).
-compile(export_all).

get(App, Key, Default) ->
   case application:get_env(App,Key) of
      undefined -> Default;
      {ok,V} -> V end.

get(Key, Default) ->
   case application:get_env(Key) of
      undefined -> Default;
      {ok,V} -> V end.
