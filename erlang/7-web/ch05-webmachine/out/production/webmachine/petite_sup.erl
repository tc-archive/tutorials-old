-module(petite_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

  Web = {
    webmachine_mochiweb, {
      webmachine_mochiweb,
      start,
      [petite_config:web_config()]
    },
    permanent,
    5000,
    worker,
    [mochiweb_socket_server]
  },

  UrlServer = {
    petite_url_srv,
    {petite_url_srv, start_link, []},
    permanent,
    5000,
    worker,
    []
  },

  Processes = [Web, UrlServer],

  {ok, {{one_for_one, 10, 10}, Processes}}.
