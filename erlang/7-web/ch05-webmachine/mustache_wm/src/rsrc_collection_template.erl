%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_collection_template).
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

  Template =
    "<html><body>" ++
    "<ul>" ++
    "{{#urls}}" ++
    "<li>{{url}}</li>" ++
    "{{/urls}}" ++
    "</ul>" ++
    "</body></html>",

  Urls = [
    {url, "https://pragprog.com/"},
    {url, "https://github.com/basho/webmachine"},
    {url, "https://github.com/mojombo/mustache.erl"}
  ],

  Dicts = [dict:from_list([U]) || U <- Urls],

  Context = dict:from_list([{urls, Dicts}]),

  Response = mustache:render(Template, Context),

  {Response, ReqData, State}.