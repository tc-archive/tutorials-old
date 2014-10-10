-module(mustache_wm_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  lists:flatten([
    {[], rsrc_mustache, []},
    {["basic"], rsrc_basic_template, []},
    {["list"], rsrc_collection_template, []},
    {["file"], rsrc_template_file, []}
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