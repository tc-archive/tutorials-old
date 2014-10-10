%%-*- mode: erlang -*-
{application, hello_wm,
 [
  {description, "hello_wm"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  inets,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { hello_wm_app, []}},
  {env, [
      {web_ip, "0.0.0.0"},
      {web_port, 8080}
  ]}
 ]}.
