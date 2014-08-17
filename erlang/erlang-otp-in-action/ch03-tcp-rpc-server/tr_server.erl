%%%----------------------------------------------------------------------------
%%% @author Martin & Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%%
%%% @author TRJL - Modifications.
%%%
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%----------------------------------------------------------------------------

-module(tr_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).


%%%============================================================================
%%% API
%%%============================================================================


%%%----------------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%%
%% @end
%% 
%% EDoc TypeSpec Attributes
%%
%% Type names always look like function calls, as in integer(), so that
%% they aren’t confused with atoms. Types can be attached directly  to
%% variables with the '::'' notation (as with Port), or they can be 
%% listed at the end of the specification as with the 'where' noatation 
%% (as with Pid = pid() ...)
%%
%%%----------------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Calls `start_link(Port)' using the default port.
%% @spec start_link() -> {ok, Pid}
start_link() ->
    start_link(?DEFAULT_PORT).

%%%----------------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%%----------------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%%----------------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%%----------------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).



%%%============================================================================
%%% GenServer Callbacks
%%%============================================================================

%% init -----------------------------------------------------------------------
init([Port]) ->
    % Use the 'gen_tcp' library to open a port.
    % 
    % A 'listening socket' is a socket that you create and wait on to accept 
    % incoming TCP connections. After you accept a connection, you have an 
    % active socket from which you can receive TCP datagrams. You pass the 
    % option {active, true}, which tells gen_tcp to send any incoming TCP data 
    % directly to your process as messages.
    %
    % TCP sockets: an active socket like this forwards all incoming data as 
    % messages to the process that created it. (With a passive socket, you’d have 
    % to keep asking it if there is more data available.)
    %
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),

    % Return from init/1 with a 3-tuple containing the atom ok, your process 
    % state (in the form of a #state{} record), and a 0 timeout.
    %
    % The 0 timeout informas the gen_server container that immediately after 
    % init/1 has finished, a timeout should be triggered that forces you to 
    % handle a timeout message (in handle_info/2) as the first thing you do 
    % after initialization.
    %
    % When a gen_server has set a timeout, and that timeout triggers, an 
    % 'out-of-band' message with the single atom 'timeout' is generated, and the 
    % handle_info/2 callback is invoked to handle it. This mechanism is usually 
    % used to make servers wake up and take some action if they have received no 
    % requests within the timeout period.
    %
    % This code is abusing this timeout mechanism slightly (it’s a well-known 
    % trick) to allow  the init/1 function to finish quickly so that the caller 
    % of start_link(...) isn’t left hanging; but at the same time, you’re making 
    % sure the server immediately jumps to a specific piece of code (the timeout 
    % clause of handle_info/2).
    % 
    {ok, #state{port = Port, lsock = LSock}, 0}.


%% handle_call ----------------------------------------------------------------
handle_call(get_count, _From, State) ->
    % Handles the 'get_count' message.
    %
    % Return the current number of requests from the 'State'; and the 'State' 
    % itself (which is unchanged).
    {reply, {ok, State#state.request_count}, State}.


%% handle_cast ----------------------------------------------------------------
handle_cast(stop, State) ->
    % Handles the 'stop' message.
    %
    % tells the gen_server container that it should stop D (that is, terminate),
    % and that the reason for termination is normal, which indicates a graceful 
    % shutdown.
    %
    % NB: the atom stop returned in this tuple instructs the container to shut 
    % down, whereas the stop message used in the protocol between the API and 
    % the server could have been any atom (such as quit), but was chosen to 
    % match the name of the API function stop().
    %
    {stop, normal, State}.


%% handle_info ----------------------------------------------------------------

%% These are considered out-of-band messages and can happen when your server 
%% needs to communicate with some other component that relies on direct messages 
%% rather than on OTP library calls—for example, a socket or a port driver. 
%% But you should avoid sending out-of- band messages to a gen_server if you can 
%% help it.

% 'tcp' message handling clause.
%
% This is the kind of message that an active socket sends to its owner when it 
% has pulled data off the TCP buffer. The RawData field contains the data.
%
handle_info({tcp, Socket, RawData}, State) ->

    % Process the TCP data as an RPC call.
    do_rpc(Socket, RawData),
    % Calculate the new 'request count'.
    RequestCount = State#state.request_count,
    % Async Return - the new updated State.
    {noreply, State#state{request_count = RequestCount + 1}};

% 'timeout' message handling clause.
handle_info(timeout, #state{lsock = LSock} = State) ->

    % Set the socket to 'accept' - NB: Invoked by immediate 'timeout' event on 
    % 'init'.
    %
    % Use gen_tcp:accept/1 to wait for a TCP connection on your listening socket 
    % (and the server will be stuck here until that happens). 
    % 
    {ok, _Sock} = gen_tcp:accept(LSock),
    % After a connection is made, the timeout clause returns and signals to the 
    % gen_server container that you want to continue as normal with an unchanged 
    % state.
    {noreply, State}.

%% terminate ------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% code_change ----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%============================================================================
%%% Internal functions
%%%============================================================================

do_rpc(Socket, RawData) ->
    try
        {M, F, A} = split_out_mfa(RawData),
        Result = apply(M, F, A),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} =
        re:run(MFA,
               "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
                   [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.


%% test

start_test() ->
    {ok, _} = tr_server:start_link(1055).
