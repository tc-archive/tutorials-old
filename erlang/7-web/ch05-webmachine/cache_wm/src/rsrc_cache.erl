%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_cache).
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

expires(ReqData, State) ->
  {{{2013, 6, 15}, {05, 11, 00}}, ReqData, State}.

to_html(ReqData, State) ->
  {"<html><body>Hello! I am Erlang Webmachine!</body></html>\n", ReqData, State}.

%%%============================================================================
%%% Manual Testing (cache on time stamp. NB: Also see ETag)
%%%============================================================================

%% $> curl -i http://localhost:8080/cache
%%
%% HTTP/1.1 200 OK
%% Server: MochiWeb/1.1 WebMachine/1.10.0
%% Last-Modified: Wed, 12 Jun 2013 22:42:00 GMT Date: Thu, 13 Jun 2013 04:47:25 GMT
%% Content-Type: text/html
%% Content-Length: 43
%% <html><body>Hello, new world</body></html>
%%
%% $> curl -i --header 'if-modified-since: Wed, 12 Jun 2013 22:42:00 GMT' http://localhost:8080/cache
%%
%% HTTP/1.1 304 Not Modified
%% Server: MochiWeb/1.1 WebMachine/1.10.0
%% Date: Thu, 13 Jun 2013 04:47:30 GMT
%%