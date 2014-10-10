%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_basic_template).
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

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  Template = "<html><body>Visit {{ url }}</body></html>",
  Context = dict:from_list([{url, "https://pragprog.com/"}]),
  Response = mustache:render(Template, Context),
  {Response, ReqData, State}.
