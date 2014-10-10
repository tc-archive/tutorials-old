-module(hello_wm_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  lists:flatten([
    {[], rsrc_hello, []},
    {["nonexistant"], rsrc_nonexistant, []},
    {["previously_existed"], rsrc_previously_existed, []},
    {["moved_permanently"], rsrc_moved_permanently, []}
  ]).

web_config() ->
  {ok, App} = application:get_application(?MODULE),
  {ok, Ip} = application:get_env(App, web_ip),
  {ok, Port} = application:get_env(App, web_port),
  [
    {ip, Ip},
    {port, Port},
    {log_dir, "priv/log"},
    {dispatch, dispatch()}
  ].