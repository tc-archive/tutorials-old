%%%============================================================================
%%% @doc 
%%% The processes that store 'cached values' in the 'simple_cache' application.
%%%
%%% Each cached value is stored as a 'sc_element' GenServer process
%%% @end
%%%============================================================================
-module(sc_element).

%%%============================================================================
%%% OTP GenServer Behaviour
%%%============================================================================

-behaviour(gen_server).

-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).

%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([
  start_link/2,
  create/2,
  create/1,
  fetch/1,
  replace/2,
  delete/1
]).

%% Constant
%%
-define(SERVER, ?MODULE).

%% Constant - The default time (one day in seconds) a cached value can remain in 
%% the cahce.
%%
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

%% Record  - Holds the cached value.
%% value       - The value the process is holding on to, 
%% lease_time  - The lease time
%% timestamp   - The timestamp from  when the process was started.
%%
-record(state, {value, lease_time, start_time}).