%%%============================================================================
%%% @doc 
%%% A gen_server whose only job is to start up, run for a while, and shut down.
%%% @end
%%%============================================================================

-module(die_please).

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

-export([start_link/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

%% GenServer name is that of the module.
-define(SERVER, ?MODULE).

%% Sleep for two seconds.
-define(SLEEP_TIME, (2*1000)).


%%%============================================================================
%%% Records
%%%============================================================================

-record(state, {}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Start a new GenServer process.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Start a new GenServer process with a timeout of SLEEP_TIME in milliseconds.
init([]) ->
  {ok, #state{}, ?SLEEP_TIME}.

% Synchronous dummy method.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% Asynchronous dummy method.
handle_cast(_Msg, State) ->
  {noreply, State}.

% Timeout handler.
handle_info(timeout, State) ->
  i_want_to_die = right_now,
  {noreply, State}.

% Terminate
terminate(_Reason, _State) ->
  ok.

% Terminate
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





