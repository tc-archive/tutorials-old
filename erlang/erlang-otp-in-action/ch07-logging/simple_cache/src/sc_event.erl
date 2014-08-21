%%%============================================================================
%%% @doc 
%%% A barebones implementation of a gen_event behaviour for the error logger.
%%% @end
%%%============================================================================

-module(sc_event).


%%%============================================================================
%%% OTP GenEvent Behaviour
%%%============================================================================

% -behaviour(gen_event).


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/0,
  add_handler/2,
  delete_handler/2
]).

-export([
  lookup/1,
  create/2,
  replace/2,
  delete/1
]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% OTP GenEvent Callbacks
%%%============================================================================

start_link() ->
  % Hide gen_event start function.
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  % Hides gen_event handler registration.
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  % Hides gen_event handler de-registration.
  gen_event:delete_handler(?SERVER, Handler, Args).

lookup(Key) ->
  % API function.
  gen_event:notify(?SERVER, {lookup, Key}).

create(Key, Value) ->
  % API function
  gen_event:notify(?SERVER, {create, {Key, Value}}).

replace(Key, Value) ->
  % API function.
  gen_event:notify(?SERVER, {replace, {Key, Value}}).

delete(Key) ->
  % API function.
  gen_event:notify(?SERVER, {delete, Key}).




