%%%============================================================================
%%% @doc 
%%% A barebones implementation of a gen_event behaviour for the error logger.
%%% @end
%%%============================================================================

-module(custom_error_report).

%% *** There’s no stopping ***
%%
%% The callback functions of a 'gen_server' can return a stop value, telling the 
%% server process to shut down. A 'gen_event' callback can’t do that (because 
%% other registered handlers may not be happy if you kill the entire service). 
%%
%% Instead, the callback can return a 'remove_handler' value, which causes the 
%% 'gen_event' process to remove that handler, calling its terminate callback 
%% function as a last favor.
%%

%% Compile the module as normal and then call its API function 
%% 'custom_error_report:register_with_logger()'' to make it hook itself into 
%% the 'error-logger' event stream.
%%

%%%============================================================================
%%% OTP GenEvent Behaviour
%%%============================================================================

-behaviour(gen_event).

-export([
  init/1, 
  handle_event/2, 
  handle_call/2,
  handle_info/2, 
  terminate/2, 
  code_change/3
]).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([register_with_logger/0]).


%%%============================================================================
%%% Records
%%%============================================================================

-record(state, {}).

%%%============================================================================
%%% Public API Implementation
%%%============================================================================

% Register this customer GenEvent process with the 'error_logger'
register_with_logger() ->
  % Add this GenEvent process as 'report handler' with the 'error_logger'.
  error_logger:add_report_handler(?MODULE).

init([]) ->
  {ok, #state{}}.

% Default 'do nothing' implementation.
handle_event(_Event, State) ->
  {ok, State}.

% Default 'do nothing' implementation.
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

% Default 'do nothing' implementation.
handle_info(_Info, State) ->
  {ok, State}.


% Default 'do nothing' implementation.
terminate(_Reason, _State) ->
  ok.

% Default 'do nothing' implementation.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





