-module(cfg).
-compile(export_all).
-include_lib("cmon/include/logger.hrl").

-define(PATH, "cfg").
-define(ETS_NAME, cfg).


ensure_ets_table() ->
   case ets:info(?ETS_NAME) of
      undefined -> ets:new(?ETS_NAME, [set,named_table]);
      _  -> ?ETS_NAME
   end.

handle_module_cfg(Module, []) ->
   Path = filename:join("cfg", Module),
   {ok, [Cfg]} = file:consult(Path),
   ets:insert(?ETS_NAME, {Module, Cfg}),
   ?INFO("Loaded config for ~p, path: ~p, data: ~p", [Module, Path, Cfg]),
   Cfg;
handle_module_cfg(Module, [{Module, PropList}]) -> PropList.

ensure_module_cfg(Module) ->
   handle_module_cfg( Module, ets:lookup(?ETS_NAME, Module) ).
   
get_m(Module, Key) -> 
   Cfg = ensure_module_cfg(Module),
   Value = proplists:get_value(Key, Cfg),
   Value.

get_m(Module, Key, Default) ->
   ensure_ets_table(),
   Cfg = ensure_module_cfg(Module),
   Value = proplists:get_value(Key, Cfg, Default),
   Value.


