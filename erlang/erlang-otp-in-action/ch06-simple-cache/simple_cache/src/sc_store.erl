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