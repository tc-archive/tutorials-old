%%%============================================================================
%%% @doc 
%%% The ti_server module is the connection handler where you accept connections 
%%% on the listening socket and bind them to a dedicated socket so that you can 
%%% start talking directly to the calling client over TCP. The strategy here, 
%%% as illustrated by figure 11.1, is to let the simple-one-for-one supervisor 
%%% keep track of the listening socket and hand it out to each new handler that 
%%% it spawns. 
%%% 
%%% The latest spawned handler is the only one that is actively listening on 
%%% the socket. As soon as it gets a connection, it tells the supervisor to 
%%% start another handler to do the listening, so it can continue processing 
%%% the accepted connection. After it’s done that, it’ll never go back to a 
%%% listening state again; it’ll die when its session ends.
%%%
%%% @end
%%%============================================================================
-module(ti_server).

%%%============================================================================
%%% OTP GenServer Behaviour
%%%============================================================================

-behaviour(gen_server).

-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([start_link/1]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).



%%%============================================================================
%%% Private State
%%%============================================================================

%% lsock  - A TCP Sockets
%%
-record(state, {lsock}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


%%%============================================================================
%%% OTP GenServer Callback Implementation
%%%============================================================================

%%% Setting server timeouts
%%%
%%% Remember that if you forget to return a new timeout value in one of the 
%%% callback functions, the timeout will revert to infinity. When you’re using 
%%% server timeouts, it’s important to remember to set them in every clause of 
%%% every callback function.
%%%

%% The start_link/1 function is how the supervisor starts each handler process, 
%% passing on the listening socket. 
%% 
%% This is propagated via 'gen_server:start_link/3' to the gen_server callback 
%% 'init/1', which stores the socket in the server state and then returns, 
%% signaling a timeout of 0 to finish the startup without keeping the caller 
%% of init/1 waiting. 
%%
%% The zero timeout makes the new gen_server process drop immediately into the 
%% timeout clause of 'handle_info/2' (timeout, State).
%%
init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.


handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.


handle_cast(stop, State) ->
  {stop, normal, State}.

%% Handle the 'tcp' message from the socket.
%%
handle_info({tcp, Socket, RawData}, State) ->
  NewState = handle_data(Socket, RawData, State),
  {noreply, NewState};
%% Handle the 'tcp_closed' message from the socket to  ensure that the 
%% ti_server process goes away automatically when the socket is closed.
%%
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
%% Initialise a new accepting socket. (Utilises 'timeout invocation trick').
%%
%% The handler process has detached from the process that called 
%% 'ti_server:start_link/1' and is running concurrently with any previously 
%% started handlers that haven’t already finished. 
%% 
%% The handler immediately calls gen_tcp:accept/1 on the listening socket, 
%% which blocks until the next incoming TCP connection. (It’s because of 
%% this blocking call that you need to ensure that nobody else is currently 
%% waiting for this process, or you’d be holding them up as well.)
%%
%% When accept() returns (this could happen almost immediately on a heavily 
%% loaded server, or after many months if the interface is rarely used), 
%% the first thing to do is to ask the supervisor to start another handler 
%% by 'calling ti_sup:start_child()'. The new handler — a clone of this one — 
%% immediately starts waiting for the next connection on the listening 
%% socket, while the current handler process can get on with handling the 
%% connection that it has accepted.
%%
%% The listening socket was opened in active mode (ti_app:start/2), so the 
%% dedicated socket returned by accept() inherits this setting. Therfore, all 
%% incoming data on the dedicated socket is sent directly and automatically 
%% to the handler process as a message of the form {tcp, Socket, RawData}.
%% 
handle_info(timeout, #state{lsock = LSock} = State) ->
  % Blocking Wait: Accept a new remote TCP connection.
  {ok, _Sock} = gen_tcp:accept(LSock),
  % Create a new server processto await the next remote TCP connection 
  % attempt.
  ti_sup:start_child(),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%%============================================================================
%%% Private Functions
%%%============================================================================

%% Currently just echos the RawDataback to the client.
%%
handle_data(Socket, RawData, State) ->
  gen_tcp:send(Socket, RawData),
  State.





