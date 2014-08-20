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

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
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