%%%============================================================================
%%%
%%% @doc The OTP App for the 'simple_cache' application
%%% @end
%%%============================================================================
-module(sc_app).

%%%============================================================================
%%% OTP Application Behaviour
%%%============================================================================

-behaviour(application).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start/2, stop/1]).


%%-----------------------------------------------------------------------------
%% @doc Start the 'simple cache' application.
%%
%% @spec start(_StartType::any(), _StartArgs::any()) -> {ok, Other}
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->

  % Register with the Erlang cluster... if not ok; fail.
  ok = init_cluster(),

  % Initialise the 'sc_store' module (currently an ETS table.)
  sc_store:init(),

  % Start the 'application root superviser'
  case sc_sup:start_link() of

    {ok, RootSupervisorPid} ->
      {ok, RootSupervisorPid};

    Other ->
      {error, Other}

  end.

%%-----------------------------------------------------------------------------
%% @doc Stop the 'simple cache' application.
%%
%% @spec stop() -> ok
%% where
%%  ok = atom()
%%
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
  ok.


%%%============================================================================
%%% Private Functions
%%%============================================================================



%%-----------------------------------------------------------------------------
%% @doc 
%% 
%% Initialise the cluster.
%% 
%% A simple method for automatically adding a new node to a predefined cluster 
%% is to always have two known blank Erlang nodes running. These are nodes 
%% without any user-defined code running on them (so there should be little 
%% reason for them to go down, ever). 
%% 
%% You start them as normal, giving them suitable names and setting the cookie 
%% to be used for authentication within this cluster:
%% 
%% > erl –name contact1 -setcookie xxxxxxxx
%% > erl –name contact2 -setcookie xxxxxxxx
%% 
%% Each cache node you bring up is configured to ping both of these nodes 
%% using their known names.If either of these calls to net_adm:ping/1 succeeds, 
%% then the node startup is allowed to proceed; if not, the node can’t join the 
%% cluster, and startup fails with a crash dump.
%%
%%
%% This proceeds as follows:
%%
%% - Check the configuration for nodes to contact (or use hardcoded defaults).
%%
%% - Ping all the contact nodes, and proceed only if you get answers.
%%
%% - Check the configuration for the time to wait for other nodes to connect 
%%   (or use a default).
%%
%% - Wait until you see more nodes than the ones that originally answered (or 
%%   you grow bored and assume you’re the first proper work node to connect).
%%
%% @end
%%
init_cluster() ->
  % Define the hard-coded default cluster managers (at least two).
  DefaultNodes = ['contact1@localhost', 'contact2@localhost'],
  % Get the configuration for the default cluster manager pair.
  case get_env(simple_cache, contact_nodes, DefaultNodes) of
￼   [] ->
      % If there is no cluster manager configurations. Error.
      {error, no_contact_nodes};
    ContactNodes ->
      % Otherwise, ensure that contact can be made with a cluster manager node.
      init_cluster(ContactNodes)
  end.


init_cluster(ContactNodes) ->
  % For each ContactNode; 'ping' that node, and ensure is it 'online'.
  % Collect a list of results.
  ReachableNodes = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
￼￼case ReachableNodes of
    [] ->
      % If no ClusterManager nodes are 'reachable' return an error.
      {error, no_contact_nodes_reachable};
    _ ->
      % Check the config for the time to wait.
      DefaultTime = 6000,
      WaitTime = get_env(simple_cache, wait_time, DefaultTime),
      % Wait for all 'reachable' ClusterManager nodes to register with 'this'  
      % node (with timeout).
      wait_for_nodes(length(ReachableNodes), WaitTime)
  end.


%%-----------------------------------------------------------------------------
%% @doc 
%% Enter a wait loop to allow the nodes to initialise.
%% @end
%%
wait_for_nodes(NumReachableNodes, WaitTime) ->
  Slices = 10,
  SliceTime = round(WaitTime/Slices),
  wait_for_nodes(ReachableNodes, SliceTime, Slices).


%% When the acummulator reaches 0; end the wait. Return ok.
wait_for_nodes(_NumReachableNodes, _SliceTime, 0) ->
  ok;

%% Wait for all 'ReachableNodes' to be registered with 'this' node.
wait_for_nodes(NumReachableNodes, SliceTime, Iterations) ->
  case length(nodes()) > NumReachableNodes of
    true -> 
      % If the number of nodes known by this node is greater than the number 
      % of reachable nodes; then finish.
      ok;
    false ->
      % Otherwise wait for the specified amont of time for the 'reachable nodes' 
      % to be registered with 'this' node.
      timer:sleep(SliceTime),
      wait_for_nodes(NumReachableNodes, SliceTime, Iterations - 1)
  end.


%%-----------------------------------------------------------------------------
%% @doc 
%% Return the environment configuration specified by the key; or return the 
%% default.
%% @end
%%
get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined -> 
      Default;
    {ok, Value} ->
       Value
  end.




