%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_never_auth).

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
  init/1,
  is_authorized/2,
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

-spec init(list()) -> {ok, term()}.
init([]) ->
  {ok, undefined}.


is_authorized(ReqData, State) ->
  {"Basic realm=testing", ReqData, State}.


-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
  {"<html><body>Hello! I is Erlang Auth Webmachine!</body></html>\n", ReqData, State}.


%%%============================================================================
%%% Manual Testing (not authed)
%%%============================================================================

%% $ curl -i http://localhost:8080/never
%%
%% HTTP/1.1 401 Unauthorized
%% WWW-Authenticate: Basic realm=testing
%% Server: MochiWeb/1.1 WebMachine/1.10.0
%% Date: Tue, 18 Jun 2013 03:13:17 GMT
%% Content-Type: text/html
%% Content-Length: 159