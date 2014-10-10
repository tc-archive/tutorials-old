%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(rsrc_petite_fetch).
-author("Temple").

%%%============================================================================
%%% Webmachine Public API
%%%============================================================================

-export([
  init/1,
  to_html/2,
  resource_exists/2,
  previously_existed/2,
  moved_permanently/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%%%============================================================================
%%% Webmachine Public API Implementation
%%%============================================================================

init([]) ->
  {ok, ""}.

to_html(ReqData, State) ->
  {"", ReqData, State}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  Code = wrq:path_info(code, ReqData),
  case petite_url_srv:get_url(Code) of
    {ok, Url} ->
      {true, ReqData, Url};
    {error, not_found} ->
      {false, ReqData, State}
  end.

moved_permanently(ReqData, State) ->
  {{true, petite_url_srv:get_url(State)}, ReqData, State}.


%%%============================================================================
%%% Manual Testing
%%%============================================================================

%% erl 1> whereis(petite_url_srv).
%% erl 2> petite_url_srv:put_url("https://pragprog.com/").
%% erl 3> petite_url_srv:put_url("https://github.com/basho/webmachine").
%%
%% erl 4> petite_url_srv:get_url("0"). % => Found
%% erl 5> petite_url_srv:get_url("1"). % => Found
%% erl 6> petite_url_srv:get_url("2"). % => Not Found
%%
%%
%% $> curl -i http://localhost:8080/0 (301 - Moved Permanently)
%% $> curl -i http://localhost:8080/1 (301 - Moved Permanently)
%% $>curl -i http://localhost:8080/2 (404 Object Not Found)

