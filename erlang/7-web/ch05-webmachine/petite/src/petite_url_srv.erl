%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2014 22:47
%%%----------------------------------------------------------------------------
-module(petite_url_srv).
-author("Temple").

%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/0,
  get_url/1,
  put_url/1
]).

%%%============================================================================
%%% OTP GenServer Behaviour
%%%============================================================================

-behaviour(gen_server).
-export([
  init/1, terminate/2,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

%%%============================================================================
%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).
-define(TAB, petite_urls).

%%%============================================================================
%%% Records and Types
%%%============================================================================

-record(st, {next}).

%%%============================================================================
%%% Public API Implementation
%%%============================================================================

start_link() ->
  Res = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  % init_default_urls(), % Have to initialise here, after state is initialised.
  Res.

get_url(Id) ->
  gen_server:call(?SERVER, {get_url, Id}).

put_url(Url) ->
  gen_server:call(?SERVER, {put_url, Url}).

%%%============================================================================
%%% GenServer Callback Implementation
%%%============================================================================

init(_) ->
  % ets:new(?TAB, [set, named_table, protected]),
  ets:new(?TAB, [set, named_table, protected]),
  {ok, #st{next=0}}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_call({get_url, Id}, _From, State) ->
  Reply =
    case ets:lookup(?TAB, Id) of
      [] ->
        {error, not_found};
    [{Id, Url}] ->
      {ok, Url}
    end,
    {reply, Reply, State};
handle_call({put_url, Url}, _From, State = #st{next=N}) ->
  Id = b36_encode(N),
  ets:insert(?TAB, {Id, Url}),
  {reply, {ok, Id}, State#st{next=N+1}};
handle_call(_Request, _From, State) ->
  {stop, unknown_call, State}.


handle_cast(_Request, State) ->
  {stop, unknown_cast, State}.


handle_info(_Info, State) ->
  {stop, unknown_info, State}.

%%%============================================================================
%%% Private Functions
%%%============================================================================

b36_encode(N) ->
  integer_to_list(N, 36).

init_default_urls() ->
  io:format("Initisalising..~n"),
  {ok,"0"} = put_url("https://pragprog.com/"),
  {ok,"1"} = put_url("https://github.com/basho/webmachine"),
  io:format("Initialised!~n"),
  ok.