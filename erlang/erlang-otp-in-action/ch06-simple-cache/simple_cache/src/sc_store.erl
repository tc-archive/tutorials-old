%%%============================================================================
%%% @doc 
%%% A storage mechanism that maps 'cache keys' to the 'process identifiers' of 
%%% the 'cache values' that ares tored against them.
%%%
%%% Erlang Term Storage is used to implemented this mapping.
%%% @end
%%%============================================================================

-module(sc_store).


%%%============================================================================
%%% Public Interface
%%%============================================================================


%% The API consists of an init/1 function for initializing the storage system 
%% and three functions that handle the basic CRUD operations (create, read, 
%% update, and delete), where the insert/2 function is used both to create new 
%% entries and to update existing ones.
-export([
  init/0,
  insert/2,
  delete/1,
  lookup/1
]).

-define(TABLE_ID, ?MODULE).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Creates a new ETS table named 'sc_store'.
init() ->
  % Create a new 'ets named table' called 'sc_store'.
  ets:new(?TABLE_ID, [public, named_table]),
  ok.


insert(Key, Pid) ->
  % In ETS There can only be one entry at a time for any specific key, so  
  % inserting a new tuple using an existing key overwrites the previous entry.
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] 
      -> {ok, Pid};
    []           
      -> {error, not_found}
  end.


delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).




