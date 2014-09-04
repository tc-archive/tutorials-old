%%%============================================================================
%%%
%%% @doc 
%%% The OTP 'Root Supervisor' for the 'tcp_interface' application.
%%%
%%% This supervisor is in many ways just a factory for 'tl_server' processes.
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
%%% OTP Supervisor Callback Implemtations
%%%============================================================================

%% Initialise the RootSuperviser process with one child processes spec:
%% 1) 'tl_server' : A 'tl_server' instance.
%%
%% This is creating a 'Superviser Tree Hierarhcy'. Following this pattern, you 
%% can nest supervisors to any depth you want to give your application a suitably 
%% fine-grained supervision structure.
%%
init([]) ->

  % Define the 'sc_element_sup' supervisor process.
  %
  TLServerSpec = {
    tl_server,                          % The Id to identify the child specification.
    {tl_server, start_link, []},        % The apply(M, F, A) tuple to start the process.
    permanent,                          % Child process always restarted.
    2000,                               % Terminate child: 'exit(Child, shutdown)' timeout.
    supervisor,                         % Child is supervisor process.
    [tl_server]                         % The name of the callback module.
  },


  ManagedProcSpecs = [TLServerSpec],

  % Strategy : 'one_for_one'
  % 
  % If a child process terminates, only that process is restarted.
  %
  %
  % Maximum Restart Frequency : (4, 3600)
  % 
  % If more than '4' restarts occur in the last '3600' seconds, then the supervisor 
  % terminates all the child processes and then itself.
  %
  ProcRestartStrategy = {one_for_one, 4, 3600},

  {ok, {ProcRestartStrategy, ManagedProcSpecs}}.



