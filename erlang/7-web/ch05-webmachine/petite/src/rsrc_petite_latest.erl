%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_petite_latest).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
  init/1,
  content_types_provided/2,
  to_text/2,
  to_html/2,
  to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, undefined}.


content_types_provided(ReqData, State) ->
  io:format("content_types_provided...~n"),
  {
    [
      {"text/html", to_html},
      {"text/plain", to_text},
      {"application/json", to_json}
    ],
    ReqData, State
  }.


to_html(ReqData, State) ->

  io:format("to_html...~n"),

  % Extract the template from the file...
  {ok, TemplateBin} =
    file:read_file(code:priv_dir(petite) ++ "/latest.html.mustache"),

  % Convert the binary to a string...
  TemplateStr = binary_to_list(TemplateBin),
  % io:format("TemplateStr >>> ~n~p~n", [TemplateStr]),

  % Determine the 'base url' of the urls...
  BaseUrl = "http://" ++ wrq:get_req_header("host", ReqData) ++ "/",

  % Get the last 20 links, and, create a 'list of single entry dicts' where
  % each entry is a 2-tuple  with the base url expanded 'short_link' as
  % the 'key' and the the base url expanded 'long_link' as the 'value'.
  {ok, LatestLinks} = petite_url_srv:get_latest(20),
  LatestDicts = [
    dict:from_list([{short_link, BaseUrl ++ ShortLink}, {long_link, LongLink}])
      || {ShortLink, LongLink} <- LatestLinks
    ],

  % Create Mustache context. Embed the list dicts in a new dict keyed on the
  % 'links' atom.
  Context = dict:from_list([{links, LatestDicts}]),
  % io:format("Context >>> ~n~p~n", [Context]),

  % Render and return the response...
  {mustache:render(TemplateStr, Context), ReqData, State}.


to_text(ReqData, State) ->

  io:format("to_text...~n"),

  {ok, LatestLinks} = petite_url_srv:get_latest(20),

  Result = lists:map(
    fun({Code, Link}) ->
      [base_url(ReqData), Code, " ", Link, "\n"]
    end,
    LatestLinks),

  {Result, ReqData, State}.


to_json(ReqData, State) ->

  io:format("to_json...~n"),

  {ok, LatestLinks} = petite_url_srv:get_latest(20),

  LinkList = lists:map(
    fun({Code, Link}) ->
      ShortLink = base_url(ReqData) ++ Code,
      {struct, [{<<"short_link">>, list_to_binary(ShortLink)},
        {<<"long_link">>, list_to_binary(Link)}]}
    end,
    LatestLinks),

  Result = mochijson2:encode({struct, [{latest, LinkList}]}),

  {[Result, "\n"], ReqData, State}.


%%%============================================================================
%%% Private Function Implementation
%%%============================================================================

base_url(ReqData) ->
  Host = wrq:get_req_header("host", ReqData),
  "http://" ++ Host ++ "/".


%%%============================================================================
%%% Manual Testing (Add shortened urls)
%%%============================================================================

%% Populate ('shorten' api)
%%
%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Fpragprog.com%2F'
%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Ferlang.com%2F'
%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Fwobble.com%2F'
%%
%%
%% Get as 'text/html' ('latest' api)
%%
%% curl --header 'accept: text/html' http://localhost:8080/latest
%%
%%
%% Get as 'text/plain' ('latest' api)
%%
%% curl --header 'accept: text/plain' http://localhost:8080/latest
%%
%%
%% Get as 'application/json' ('latest' api)
%%
%% curl --header 'accept: application/json' http://localhost:8080/latest