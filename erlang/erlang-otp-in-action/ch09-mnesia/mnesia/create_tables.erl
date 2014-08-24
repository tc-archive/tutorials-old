
%%%============================================================================
%%% @doc 
%%% Create Mnesia tables for the 'simple_cache' project. 
%%% @end
%%%============================================================================

-module(create_tables).

%% Tables are created with the function mnesia:create_table(Name, Options), 
%% where Options is a list of {Name, Value} pairs. The main option that you 
%% almost always need to supply is attributes, which assigns names to the 
%% fields of the records that will be stored in the table. Without it, Mnesia 
%% assumes you’ll only have two fields in your records, named key and val, 
%% respectively.

%%%============================================================================
%%% Public API
%%%============================================================================

-export([init_tables/0]).


%%%============================================================================
%%% Records
%%%============================================================================

%%  Mnesia tables and Erlang records
%% 
%% To Mnesia, a table is just a bunch of tagged tuples. This is exactly what 
%%  Erlang’s records are (see section 2.11), but Mnesia can’t know that a table 
%% has anything to do with your –record(...) declaration of the same name. 
%% You need to set up this connection yourself. (Sometimes, it can be useful 
%% not to be forced to have a connection between a table and a record, even if 
%% they have the same name.)
%% 
%% You could hardcode the names, as in {attributes, [title, description]}; 
%% but it’s better to use record_info(fields, RecordName) to list the field 
%% names, in case you change the record declaration later. Note that 
%% record_info/2 isn’t a real function — it will be resolved at compile time (
%% just like the # syntax for records) and can’t be called at runtime or from 
%% the Erlang shell.

%% No matter what you name them, the first field of the record is always the 
%% primary key.



-record(user, {id, name }).

-record(project, {title, description}).

-record(contributor, {user_id, project_title}).


%%%============================================================================
%%% Implemented API
%%%============================================================================


%% Only specifying the 'attributes' option means that the table will get the 
%% default settings for all other options. These are as follows:
%%
%% - The table is both readable and writeable.
%% - The table is stored in RAM only (the storage type is ram_copies).
%% - The records stored in the table must have the same name as the table.
%% - The table type is set, which means there can be no more than one entry
%%   per key.
%% - The load priority is 0 (the lowest).
%% - The local_content flag is set to false.
%%
init_tables() ->
  mnesia:create_table(
    user, [{attributes, record_info(fields, user)}]
    ),
  mnesia:create_table(
    project,[{attributes, record_info(fields, project)}]
    ),
  mnesia:create_table(
    contributor, [{type, bag}, {attributes, record_info(fields, contributor)}]
    ).