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


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Create a new 'tc_element' GenServer process.
%%
%% NB: This method is called from the 'sc_sup' supervisor to allow the  
%%     management/supervision of created processes.
%%
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% Create (store) a new Value with the specified LeaseTime.
%%
%% NB: This method calls the 'sc_sup' supervisor to allow the  
%%     management/supervision of created processes. 
%%
%%     The supervisor will use the 'sc_element:start_link(Value, LeaseTime)' 
%%     (configured in the 'sc_sup:init' method) to actually create the  
%%     supervised process. This method in turn call the GenServer behviour 
%%     'start_link' message. Simples! 
%% 
create(Value, LeaseTime) ->
    sc_sup:start_child(Value, LeaseTime).

%% Create (store) a new Value with the default LeaseTime. 
create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

%% Fetch the state Value of the specified storage process. 
fetch(Pid) ->
  % Synchronously calls the specified GenServer process with a 'fetch' message  
  % to return the GenServer process 'state' (the stored 'Value').
  gen_server:call(Pid, fetch).

%% Replace the state Value of the specified storage process. 
replace(Pid, Value) ->
  % Asynchronously calls the specified GenServer process with a 'replace'   
  % message to replace the specified Value.
  gen_server:cast(Pid, {replace, Value}).

%% Delete the state Value of the specified storage process. 
delete(Pid) ->
  % Asynchronously calls the specified GenServer process with a 'delete'   
  % message to delete the specified Value.
  gen_server:cast(Pid, delete).


%%%============================================================================
%%% OTP Supervisor Callbacks
%%%============================================================================

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok, 
    #state{
      value = Value, 
      lease_time = LeaseTime, 
      start_time = StartTime
    }, 
    time_left(StartTime, LeaseTime)
  }.


time_left(_StartTime, infinity) ->
    infinity;

time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime =  calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.


handle_call(fetch, _From,  State) ->
    #state{value = Value,
           lease_time = LeaseTime,
           start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.


handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime,
           start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};


handle_cast(delete, State) ->
    {stop, normal, State}.


handle_info(timeout, State) ->
    {stop, normal, State}.


terminate(_Reason, _State) ->
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


