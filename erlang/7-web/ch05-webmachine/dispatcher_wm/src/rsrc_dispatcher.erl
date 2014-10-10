-module(rsrc_dispatcher).
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
%% Test with: curl --header 'accept: text/html' http://localhost:8080/noodle/pancakes
%% @end
%%-----------------------------------------------------------------------------
-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) -> {
  [
    <<"<html><body>">>,
    <<"You asked for ">>, wrq:path(ReqData), <<"\n">>,
    <<"</body></html>">>
  ],
  ReqData, State}.

