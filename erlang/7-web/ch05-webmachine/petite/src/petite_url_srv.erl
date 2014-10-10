%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(petite_url_srv).
-author("Temple").

%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  start_link/0,
  get_url/1,
  put_url/1,
  get_latest/1
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

%% The state of this gen_server is a simple integer. It is incremented as
%% each integer is used to as the next 'shortened url identifier'...
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

get_latest(Count) ->
  gen_server:call(?SERVER, {get_latest, Count}).

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


%% |*| - 'get_url' clause.
handle_call({get_url, Id}, _From, State) ->
  Reply =
    case ets:lookup(?TAB, Id) of
      [] ->
        {error, not_found};
    [{Id, Url}] ->
      {ok, Url}
    end,
    {reply, Reply, State};
%% |*| - 'put_url' clause.
handle_call({put_url, Url}, _From, State = #st{next=N}) ->
  Id = b36_encode(N),
  ets:insert(?TAB, {Id, Url}),
  % Update the integer state to use the next as the next 'new shortend url'...
  {reply, {ok, Id}, State#st{next=N+1}};
%% |*| - 'get_latest' clause.
handle_call({get_latest, Count}, _From, State = #st{next=N}) ->
  Start = N - 1,
  End = max(N - Count, 0),
  Ids = [b36_encode(I) || I <- lists:seq(Start, End, -1)],
  Result = lists:map(
    fun(Id) ->
      [Record] = ets:lookup(?TAB, Id),
      Record
    end,
    Ids
  ),
  {reply, {ok, Result}, State};
%% |*| - Catch all clause.
handle_call(_Request, _From, State) ->
  {stop, unknown_call, State}.


handle_cast(_Request, State) ->
  {stop, unknown_cast, State}.


handle_info(_Info, State) ->
  {stop, unknown_info, State}.

%%%============================================================================
%%% Private Functions
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @doc
%%% Accepts an integer and encode it to base 36.
%%% @end
%%%----------------------------------------------------------------------------
b36_encode(N) ->
  integer_to_list(N, 36).

init_default_urls() ->
  io:format("Initisalising..~n"),
  {ok,"0"} = put_url("https://pragprog.com/"),
  {ok,"1"} = put_url("https://github.com/basho/webmachine"),
  io:format("Initialised!~n"),
  ok.