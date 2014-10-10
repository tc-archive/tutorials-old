%%%-------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rsrc_template_file).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->

  io:format("code:priv_dir: ~p~n", [code:priv_dir(mustache_wm)]),

  {ok, TemplateBin} =
    file:read_file(code:priv_dir(mustache_wm) ++ "/simple.mustache"),

  TemplateStr = binary_to_list(TemplateBin),

  Context = dict:from_list([{message, "Hello from a file."}]),

  Response = mustache:render(TemplateStr, Context),

  {Response, ReqData, State}.
