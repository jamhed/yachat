-module(cfg).
-compile(export_all).

get(App, Key, Default) ->
   case application:get_env(App,Key) of
      undefined -> Default;
      {ok,V} -> V
   end.

get(Key, Default) ->
   case application:get_env(Key) of
      undefined -> Default;
      {ok, V} -> V
   end.

% default app to search
get_a(App, Key, Default) ->
   case application:get_env(Key) of
      undefined -> 
         case application:get_env(App,Key) of
            undefined -> Default;
            {ok, V} -> V
         end;
      {ok, V} -> V
   end.

get(Key) ->
   {ok, V} = application:get_env(Key),
   V.

% default app to search
get_a(App, Key) ->
   case application:get_env(Key) of
      undefined -> 
         case application:get_env(App, Key) of
            {ok, V} -> V;
            Default -> Default
         end;
      {ok, V} -> V
   end.
