-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).

-define(APP, web).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, _} = cowboy:start_http(http, 100, 
         [{port, 9080}],
         [
            {env, [{dispatch, dispatch_rules()}]},
            {timeout, 60}
         ]
      ),
    {ok, {{one_for_one, 5, 10}, []}}.

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/store/[...]",        web_store,        ["store"]},
            {"/image/[...]",        web_image,        ["store"]},
            {"/",                   cowboy_static,    {file, "web/index.html"}},
            {"/main/ws/[...]",      bullet_handler,   [{handler, ws_main}]},
            {"/[...]",              cowboy_static,    {dir, "web"}}
    ]}]).
