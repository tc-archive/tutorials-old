-module(rsrc_bound_param).
-export([
  init/1,
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
%% @doc The 'to_html' handler. Prtins the current path...
%% Test with: curl -i http://localhost:8080/bound_param/to_capture
%% @end
%%-----------------------------------------------------------------------------
-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) -> {
  [
    <<"<html><body>">>,
    "BoundParm: ", wrq:path_info(bound_param, ReqData),
    <<"</body></html>">>
  ],
  ReqData, State}.
