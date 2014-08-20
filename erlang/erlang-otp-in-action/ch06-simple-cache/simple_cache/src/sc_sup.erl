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


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------------------
%% @doc 
%% Start the 'simple cache' 'root superviser' process.
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%-----------------------------------------------------------------------------
%% @doc 
%% The supervisor can be asked to start new child processes at any time.
%%
%% The function asks the running supervisor (identified by ?SERVER) to start a 
%% new child, passing it the extra arguments 'Value' and 'LeaseTime'
%% @end
%%-----------------------------------------------------------------------------
start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

%%-----------------------------------------------------------------------------
%% @doc 
%% This supervisor is set up for simple_one_for_one supervision. A 
%% 'simple_one_for_one' supervisor can start only one type of child, but can 
%% start any number of them; all its children are dynamically added at runtime, 
%% and no child process is started when the supervisor starts up.
%%
%% The children are marked as temporary rather than permanent, meaning that if 
%% they die, they should not be restarted.
%%
%% The shutdown type is set to 'brutal_kill', indicating that the children 
%% should be terminated immediately when the supervisor shuts down.
%% @end
%%-----------------------------------------------------------------------------
init([]) ->

  Element = {sc_element, {sc_element, start_link, []},
             temporary, brutal_kill, worker, [sc_element]},

  Children = [Element],
    
  RestartStrategy = {simple_one_for_one, 0, 1},

  {ok, {RestartStrategy, Children}}.