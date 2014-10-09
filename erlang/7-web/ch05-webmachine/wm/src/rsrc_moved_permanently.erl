-module(rsrc_moved_permanently).

-export([init/1]).

-export([
  resource_exists/2,
  previously_existed/2,
  moved_permanently/2,
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
%% @end
%%-----------------------------------------------------------------------------
-spec previously_existed(wrq:reqdata(), term())
      -> {boolean(), wrq:reqdata(), term()}.
previously_existed(ReqData, State) ->
  {true, ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc REST State Interceptor - 'moved_permanently'.
%% Test with: curl -i http://localhost:8080/moved_permanently
%% Should fail with 301 Moved Permanently, and the new Location.
%% @end
%%-----------------------------------------------------------------------------
-spec moved_permanently(wrq:reqdata(), term())
      -> {boolean(), wrq:reqdata(), term()}.
moved_permanently(ReqData, State) ->
  {{true, "theNewUrl"}, ReqData, State}.

%%-----------------------------------------------------------------------------
%% @doc The 'to_html' handler.
%% Test with: curl -i http://localhost:8080/moved_permanently
%% Should fail with 301 Moved Permanently, and the new Location.
%% @end
%%-----------------------------------------------------------------------------
-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) -> {
  "Go away!",
  ReqData, State}.

