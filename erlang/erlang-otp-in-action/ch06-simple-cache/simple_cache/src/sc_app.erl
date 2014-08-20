%%%============================================================================
%%%
%%% @doc The OTP App for the 'simple_cache' application
%%% @end
%%%============================================================================
-module(sc_app).

%%%============================================================================
%%% OTP Application Behaviour
%%%============================================================================

-behaviour(application).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start/2, stop/1]).


%%-----------------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%%
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%%
%% @end
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->

  case sc_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
end.

%%-----------------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%%
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%%
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
    ok.