%%%----------------------------------------------------------------------------
%%% The purpose of an active application is to run one or more processes to do 
%%% some work. In order to have control over those processes, they should be 
%%% spawned and managed by supervisors: processes that implement the supervisor 
%%% behaviour.
%%%
%%% @doc The OTP Root Superviser for the TCP-RPC Server
%%% @end
%%%----------------------------------------------------------------------------

-module(tr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).


%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

  Server = {
    tr_server, {tr_server, start_link, []},
    permanent, 2000, worker, [tr_server]
    },

  Children = [Server],

  RestartStrategy = {one_for_one, 0, 1},

  {ok, {RestartStrategy, Children}}.