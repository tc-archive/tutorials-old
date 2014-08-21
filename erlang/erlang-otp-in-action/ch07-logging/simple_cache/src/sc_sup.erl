%%%============================================================================
%%%
%%% @doc 
%%% The OTP 'Root Supervisor' for the 'simple_cache' application.
%%%
%%% This supervisor is in many ways just a factory for 'sc_element' processes.
%%% @end
%%%============================================================================
-module(sc_sup).


%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([init/1]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([start_link/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%============================================================================
%%% OTP Supervisor Callbacks
%%%============================================================================

%% Initialise the RootSuperviser process.
init([]) ->

  %
  ElementSup = {
    sc_element_sup, 
    {sc_element_sup, start_link, []},
    permanent, 
    2000, 
    supervisor, 
    [sc_element]
  },

  EventManager = {
    sc_event, 
    {sc_event, start_link, []},
    permanent, 
    2000, 
    worker, 
    [sc_event]
  },

  Children = [ElementSup, EventManager],

  RestartStrategy = {one_for_one, 4, 3600},

  {ok, {RestartStrategy, Children}}.
