-module(rsrc_previously_existed).

-export([init/1]).

-export([
  resource_exists/2,
  previously_existed/2,
  to_html/2
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
%% @doc REST State Interceptor - 'resource_exists'.
%% @end
%%-----------------------------------------------------------------------------
-spec resource_exists(wrq:reqdata(), term())
      -> {boolean(), wrq:reqdata(), term()}.
resource_exists(ReqData, State) ->
  {false, ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc REST State Interceptor - 'previously_existed'.
%% Test with: curl -i http://localhost:8080/previously_existed
%% Should fail with 410 Gone.
%% @end
%%-----------------------------------------------------------------------------
-spec previously_existed(wrq:reqdata(), term())
      -> {boolean(), wrq:reqdata(), term()}.
previously_existed(ReqData, State) ->
  {true, ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc The 'to_html' handler.
%% Test with: curl -i http://localhost:8080/previously_existed
%% Should fail with 410 Gone.
%% @end
%%-----------------------------------------------------------------------------
-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) -> {
  "Go away!",
  ReqData, State}.

