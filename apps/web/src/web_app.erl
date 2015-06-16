-module(web_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> 
   dbd:check_config(),
   dbd:initialize(),
   srv_cleanup:start(),
   web_sup:start_link().

stop(_State) -> ok.
