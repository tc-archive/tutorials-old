%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_cache_control).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
  init/1,
  to_html/2,
  expires/2
]). 

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, undefined}.

%% NB: Webmachine has no 'cache-control' override, but, as it is a variation
%%     of 'expires' this hook can be used to set the required response header
%%     appropriately.
expires(ReqData, State) ->
  ReqData2 = wrq:set_resp_header("Cache-Control", "max-age=30", ReqData),
  {undefined, ReqData2, State}.


to_html(ReqData, State) ->
  {"<html><body>Hello! I am Erlang Webmachine!</body></html>\n", ReqData, State}.

