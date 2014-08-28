%%%============================================================================
%%% @doc 
%%% A storage mechanism that maps 'cache keys' to the 'process identifiers' of 
%%% the 'cache values' that ares tored against them.
%%%
%%% Mnesia is used to implemented this mapping.
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
%%% Records
%%%============================================================================

%% NB: A record is a 'named tuple' => {key_to_pid, Key, Pid}
%%
-record(key_to_pid, {key, pid}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Creates a new Mnesia table named 'key_to_pid'.
init() ->
  % The table is a normal set with unique keys, kept in RAM only. As always, 
  % Table entity field defined by 'key_to_pid' record.
  %
  % Note the option {index, [pid]} in the table definition. Indexes are extra 
  % tables that allow speedy operations on fields other than the primary key. 
  % Keep in mind when you’re creating an index that it consumes additional space.
  %
  mnesia:start(),
  mnesia:create_table(
    key_to_pid,
    [{index, [pid]}, {attributes, record_info(fields, key_to_pid)}]
  ).


insert(Key, Pid) ->
  % Using Mnesia as KV-Store, so, no need for transacitons...
  mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).
  % mnesia:transaction(fun -> mnesia:write(#key_to_pid{key=Key, pid=Pid}) end).


%% There is a complication to 'looukp' in a distributed setting, the pid you 
%% get from the lookup could be referring to a dead process. Consider the 
%% following scenario: You have nodes a and b and you insert some data in the 
%% cache on node a. You check that you can look up that data on node b and 
%% get the correct value. You then kill node a and run the same query again on 
%% node b. What happens? The operation fails, because the pid in the Mnesia 
%% database still refers to the storage process which was located on node a, 
%% but is now dead. You need a way to invalidate entries that refer to dead 
%% processes.
%%
%% So... we need to check the process defined by the mnesia key exists before 
%% fetching it. Also, we could clean up the mnesia table when the node is shutting 
%% down (in )
%%
lookup(Key) ->

  io:format("sc_store:lookup~n"),

  % Look up the tuple referenced by the key in the Mnesia table.
  %
  % The table is a set, you can only get zero or one records as result, and a 
  % dirty read is sufficient for your purposes.
  % 
  case mnesia:dirty_read(key_to_pid, Key) of
    [{key_to_pid, Key, Pid}] ->
      % Check the Pid in the Mnesia database still refers to a process is 
      % alive (on this or reote node).
      case is_pid_alive(Pid) of
        true -> 
          {ok, Pid};
        false ->
          io:format("sc_store:lookup - Pid dead ~p.~n", [Pid]),
          {error, not_found}
      end;
    [] -> 
      io:format("sc_store:lookup - []~n"),
      {error, not_found}
  end.


delete(Pid) ->
  % Use a special index based read...
  % For index_read/3:
  % - the first argument is the table name, 
  % - the second is the key on which you want to index (the pid), 
  % - the third indicates which index you want to search (because a table can have several).
  %
  case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
    % Capture the {key, pid} if it exists.
    [#key_to_pid{} = Record] ->
      % Delete the record. Return ok.
      mnesia:dirty_delete_object(Record);
    % Keep the methods idempotent.
    _ -> 
      ok
  end.



%%%============================================================================
%%% Private Functions
%%%============================================================================

%% Checks whether a Pid refers to a process that still lives. If so, returns true.
%%
is_pid_alive(Pid) when node(Pid) =:= node() ->
  % If the Pid is running on this node then check if it is alive...
  is_process_alive(Pid);
is_pid_alive(Pid) ->
  % If the Pid is not running this node, but is in part of the (remote) cluster 
  % and...
  lists:member(node(Pid), nodes()) andalso
  % ... the process is alive on remote node...
  (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).




