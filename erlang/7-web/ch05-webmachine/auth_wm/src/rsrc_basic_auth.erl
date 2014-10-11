%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_basic_auth).

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
  AuthHead = "Basic realm=Identify yourself!",
  Result =
    case wrq:get_req_header("authorization", ReqData) of
      "Basic " ++ EncodedAuthStr ->
        AuthStr = base64:decode_to_string(EncodedAuthStr),
        [User, Pass] = string:tokens(AuthStr, ":"),
        case authorized(User, Pass) of %% (3)
          true ->
            true;
          false ->
            AuthHead
        end;
      _ ->
        AuthHead
    end,
  {Result, ReqData, State}.


-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
  {"<html><body>Hello! I is Erlang Auth Webmachine!</body></html>\n", ReqData, State}.


%%%============================================================================
%%% Private Functions
%%%============================================================================

authorized(User, Pass) ->
  User =:= "test" andalso Pass =:= "12345".


%%%============================================================================
%%% Manual Testing (not authed)
%%%============================================================================

%%  $ curl -v -u test:12345 http://localhost:8080/basic
%%
%% * Adding handle: conn: 0x7fe399004000
%% * Adding handle: send: 0
%% * Adding handle: recv: 0
%% * Curl_addHandleToPipeline: length: 1
%% * - Conn 0 (0x7fe399004000) send_pipe: 1, recv_pipe: 0
%% * About to connect() to localhost port 8080 (#0)
%% *   Trying 127.0.0.1...
%% * Connected to localhost (127.0.0.1) port 8080 (#0)
%% * Server auth using Basic with user 'test'
%% > GET /basic HTTP/1.1
%% > Authorization: Basic dGVzdDoxMjM0NQ==
%% > User-Agent: curl/7.30.0
%% > Host: localhost:8080
%% > Accept: */*
%% >
%% < HTTP/1.1 200 OK
%% * Server MochiWeb/1.1 WebMachine/1.10.6 (no drinks) is not blacklisted
%% < Server: MochiWeb/1.1 WebMachine/1.10.6 (no drinks)
%% < Date: Sat, 11 Oct 2014 12:20:54 GMT
%% < Content-Type: text/html
%% < Content-Length: 62
%% <
%% <html><body>Hello! I is Erlang Auth Webmachine!</body></html>
%% * Connection #0 to host localhost left intact