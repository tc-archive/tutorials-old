%%%----------------------------------------------------------------------------
%%%
%%% @doc The OTP App for the 'simple_cache' application
%%% @end
%%%----------------------------------------------------------------------------
-module(sc_app).

%%%----------------------------------------------------------------------------
%%% Application
-behaviour(application).

%%%----------------------------------------------------------------------------
%%% Public Interface
%%%
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->

  case sc_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
end.

stop(_State) ->
    ok.