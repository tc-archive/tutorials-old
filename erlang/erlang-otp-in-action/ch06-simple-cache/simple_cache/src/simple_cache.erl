%%%============================================================================
%%% @doc 
%%%
%%% An API to the simple_cache appli- cation. This module is a set of interface 
%%% functions for clients of the  'simple_cache':
%%%
%%% - insert/2—Stores a key and corresponding value in the cache
%%%
%%% - lookup/1—Uses a key to retrieve a value
%%%
%%% - delete/1—Uses a key to delete the key/value pair from the cache
%%%
%%% NB: This API doesn’t include any functions for starting or stopping the 
%%% simple_cache; that is handled via OTP system functions such as 
%%% application:start/1.
%%%
%%% @end
%%%============================================================================

The convention for application-level API modules is to give them the same name as the application.

-module(simple_cache).

-export([insert/2, lookup/1, delete/1]).


insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.


lookup(Key) ->
  try
     {ok, Pid} = sc_store:lookup(Key),
    {ok, Value} = sc_element:fetch(Pid),
    {ok, Value}
  catch
    _Class:_Exception ->
    {error, not_found}
  end.


delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.