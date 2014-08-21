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

%% Management API
-export([
  start_link/0,
  add_handler/2,
  delete_handler/2
]).

%% Client API
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
%%% Public Management API Implementation
%%%============================================================================

%% This API module doesn’t implement any specific OTP behaviour. But it does 
%% provide a start_link() function similar to what you’re used to. In this 
%% case, it hides a call to the function gen_event:start_link/1, starting a 
%% new gen_event container and registering it locally using the same name as 
%% the module.
%%
%% Many gen_event behaviour implementation modules don’t provide a 'start_link' 
%% API function. Normally, the gen_event container (also called the event 
%% manager) is instead started directly from a supervisor, as illustrated by 
%% the following child specification example:
%%
%% {
%%    my_logger,
%%    {gen_event, start_link, [{local, my_logger}]},
%%    permanent, 
%%    1000, 
%%    worker, 
%%    [gen_event]
%% }
%%
%% After it’s started, the process can be referenced by the name 'my_logger' 
%% in order to add handlers.
%%
start_link() ->
  % Hide gen_event start function.
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  % Hides gen_event handler registration.
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  % Hides gen_event handler de-registration.
  gen_event:delete_handler(?SERVER, Handler, Args).


%%%============================================================================
%%% Public Cient API Implementation
%%%============================================================================

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




