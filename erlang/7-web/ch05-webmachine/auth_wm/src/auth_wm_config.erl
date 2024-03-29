-module(auth_wm_config).

-export([
  dispatch/0,
  web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  lists:flatten([
    {[], rsrc_auth_wm, []},
    {["never"], rsrc_never_auth, []},
    {["basic"], rsrc_basic_auth, []}
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