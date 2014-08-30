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


%%%============================================================================
%%% Public Interface
%%%============================================================================

-define(WAIT_FOR_RESOURCES, 2500).



%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

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

  % Harmonise the 'simple_cache' instances in the cluster.
  harmonise_cluster(),

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
    [] ->
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
  case ReachableNodes of
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
  wait_for_nodes(NumReachableNodes, SliceTime, Slices).


%% When the acummulator reaches 0; end the wait. Return ok.
wait_for_nodes(_NumReachableNodes, _SliceTime, 0) ->
  ok;

%% Wait for all 'ReachableNodes' to be registered with 'this' node.
wait_for_nodes(NumReachableNodes, SliceTime, Iterations) ->
  case length(nodes()) > NumReachableNodes of
    true -> 
      % If the number of nodes known by this node is greater than the number 
      % of reachable nodes; then finish.
      %
      % NB: Lets hope they are the nodes we expect!
      ok;
    false ->
      % Otherwise wait for the specified amont of time for the 'reachable nodes' 
      % to be registered with 'this' node.
      timer:sleep(SliceTime),
      wait_for_nodes(NumReachableNodes, SliceTime, Iterations - 1)
  end.


%%-----------------------------------------------------------------------------
%% @doc 
%% Use the resource_discovery module to publish the local cache as available to
%% others, and locate other cache instances in the cluster.
%% @end
%%
harmonise_cluster() ->
  
  % Register this cache as a local resource.
  resource_discovery:add_local_resource(simple_cache, node()),
  % Register 'simple_cache' as a required resource type
  resource_discovery:add_target_resource_type(simple_cache),
  % Trade this resource with other nodes in the cluster.
  resource_discovery:trade_resources(),
  % Wait for trade_resources to complete.
  timer:sleep(?WAIT_FOR_RESOURCES).



%%-----------------------------------------------------------------------------
%% @doc 
%% Return the environment configuration specified by the key; or return the 
%% default.
%% @end
%%
%%
%% Use configuration with care
%%
%% Reading configuration settings creates functions that aren’t referentially 
%% transparent: what you pass as parameters to the function isn’t the only thing 
%% that decides what it will return. It’s good functional programming practice 
%% not to bury such things deep in your code. Keep them at the top of the 
%% program (in the initialization part), and make them easily visible. (In this 
%% case, you only read configuration during the start/2 function.) Following this 
%% practice will make your code more man- ageable and more easily refactored. 
%% The more referentially transparent functions you have, the easier it is to 
%% reason about what your code does and how it will be affected if you rearrange 
%% things.
%%
get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined -> 
      Default;
    {ok, Value} ->
       Value
  end.




