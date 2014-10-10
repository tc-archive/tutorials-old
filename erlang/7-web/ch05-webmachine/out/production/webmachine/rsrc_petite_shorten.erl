%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_petite_shorten).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([init/1,
  allowed_methods/2,
  process_post/2,
  content_types_provided/2,
  to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, undefined}.


allowed_methods(ReqData, State) ->
  {['POST'], ReqData, State}.


content_types_provided(ReqData, State) ->
  {[{"text/plain", to_text}], ReqData, State}.


process_post(ReqData, State) ->
  Host = wrq:get_req_header("host", ReqData),
  Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  Url = proplists:get_value("url", Params),
  {ok, Code} = petite_url_srv:put_url(Url),
  Shortened = "http://" ++ Host ++ "/" ++ Code ++ "\n",
  {true, wrq:set_resp_body(Shortened, ReqData), State}.


%%%============================================================================
%%% Private Functions
%%%============================================================================

to_text(ReqData, State) ->
  {"", ReqData, State}.


%%%============================================================================
%%% Manual Testing (Add shortened urls)
%%%============================================================================

%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Fpragprog.com%2F'
%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Ferlang.com%2F'
%% curl -i -X POST http://localhost:8080/shorten --data 'url=https%3A%2F%2Fwobble.com%2F'