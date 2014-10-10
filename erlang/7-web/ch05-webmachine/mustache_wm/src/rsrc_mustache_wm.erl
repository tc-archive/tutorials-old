%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_mustache_wm).
-author("Temple").


%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    {"<html><body>Hello, new world</body></html>", ReqData, State}.

