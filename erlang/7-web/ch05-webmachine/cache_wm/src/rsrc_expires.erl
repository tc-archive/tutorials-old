%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_expires).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
  init/1,
  to_html/2,
  last_modified/2
]). 

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, undefined}.

last_modified(ReqData, State) ->
  {{{2013, 6, 12}, {22, 42, 00}}, ReqData, State}.

to_html(ReqData, State) ->
  {"<html><body>Hello! I am Erlang Webmachine!</body></html>\n", ReqData, State}.

