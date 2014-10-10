-module(rsrc_hello).

-export([init/1]).

-export([
  content_types_provided/2,
  to_html/2,
  to_text/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------------------------
%% @doc Initialiser.
%% @end
%%-----------------------------------------------------------------------------
-spec init(list()) -> {ok, term()}.
init([]) ->
  {ok, undefined}.

%%-----------------------------------------------------------------------------
%% @doc REST State Interceptor - 'content_types_provided'.
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_provided(wrq:reqdata(), term())
      -> {iodata(), wrq:reqdata(), term()}.
content_types_provided(ReqData, State) -> {[
  {"text/html", to_html},
  {"text/plain", to_text}
  ], ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc The 'to_html' handler.
%% Test with: curl --header 'accept: text/html' http://localhost:8080/
%% @end
%%-----------------------------------------------------------------------------
-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) -> {
  "<html><body>Hello, new world! I am WebMachine!</body></html>",
  ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc 'to_text' handler.
%% Test with: curl --header 'accept: text/plain' http://localhost:8080/
%% @end
%%-----------------------------------------------------------------------------
-spec to_text(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_text(ReqData, State) -> {
  "Hello, text world\n",
  ReqData, State}.

