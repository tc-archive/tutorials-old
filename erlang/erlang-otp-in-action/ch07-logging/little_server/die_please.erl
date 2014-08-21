%%%============================================================================
%%% @doc 
%%% A gen_server whose only job is to start up, run for a while, and shut down.
%%% @end
%%%============================================================================

-module(die_please).

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

-export([start_link/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

%% GenServer name is that of the module.
-define(SERVER, ?MODULE).

%% Sleep for two seconds.
-define(SLEEP_TIME, (2*1000)).


%%%============================================================================
%%% Records
%%%============================================================================

-record(state, {}).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

%% Start a new GenServer process.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Start a new GenServer process with a timeout of SLEEP_TIME in milliseconds.
init([]) ->
  {ok, #state{}, ?SLEEP_TIME}.

% Synchronous dummy method.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% Asynchronous dummy method.
handle_cast(_Msg, State) ->
  {noreply, State}.

% Timeout handler. Proivder no requests are issues to the server this should 
% be invoked after SLEEP_TIME ms form init.
handle_info(timeout, State) ->
  % This is illegal and will cause an error! Atomcs can never match!
  i_want_to_die = right_now,
  {noreply, State}.

% Terminate
terminate(_Reason, _State) ->
  ok.

% Terminate
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%============================================================================
%%% Example Usage
%%%============================================================================


%% *** Invoke a stack trace ***
%% 

% $> erl
% Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] 
% [kernel-poll:false] [dtrace]

% Eshell V6.1  (abort with ^G)
% 1> c("die_please.erl").
% die_please.erl:75: Warning: no clause will ever match
% {ok,die_please}
% 2> die_please:start_link().
% {ok,<0.39.0>}
% 3>
% =ERROR REPORT==== 21-Aug-2014::17:07:11 ===
% ** Generic server die_please terminating
% ** Last message in was timeout
% ** When Server state == {state}
% ** Reason for termination ==
% ** {{badmatch,right_now},
%     [{die_please,handle_info,2,[{file,"die_please.erl"},{line,75}]},
%      {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,599}]},
%      {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
% ** exception error: no match of right hand side value right_now
%      in function  die_please:handle_info/2 (die_please.erl, line 75)
%      in call from gen_server:handle_msg/5 (gen_server.erl, line 599)
%      in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 239)
% 3>

%% The server process comes up just fine, and 2 seconds later it terminates with 
%% some reasonably useful error information.


%% *** Start SASL ***
%%  

% 3> application:start(sasl).
%
% =PROGRESS REPORT==== 21-Aug-2014::17:10:46 ===
%           supervisor: {local,sasl_safe_sup}
%              started: [{pid,<0.47.0>},
%                        {name,alarm_handler},
%                        {mfargs,{alarm_handler,start_link,[]}},
%                        {restart_type,permanent},
%                        {shutdown,2000},
%                        {child_type,worker}]
%
% =PROGRESS REPORT==== 21-Aug-2014::17:10:46 ===
%           supervisor: {local,sasl_safe_sup}
%              started: [{pid,<0.48.0>},
%                        {name,overload},
%                        {mfargs,{overload,start_link,[]}},
%                        {restart_type,permanent},
%                        {shutdown,2000},
%                        {child_type,worker}]
%
% =PROGRESS REPORT==== 21-Aug-2014::17:10:46 ===
%           supervisor: {local,sasl_sup}
%              started: [{pid,<0.46.0>},
%                        {name,sasl_safe_sup},
%                        {mfargs,
%                            {supervisor,start_link,
%                                [{local,sasl_safe_sup},sasl,safe]}},
%                        {restart_type,permanent},
%                        {shutdown,infinity},
%                        {child_type,supervisor}]
%
% =PROGRESS REPORT==== 21-Aug-2014::17:10:46 ===
%           supervisor: {local,sasl_sup}
%              started: [{pid,<0.49.0>},
%                        {name,release_handler},
%                        {mfargs,{release_handler,start_link,[]}},
%                        {restart_type,permanent},
%                        {shutdown,2000},
%                        {child_type,worker}]
%
% =PROGRESS REPORT==== 21-Aug-2014::17:10:46 ===
%          application: sasl
%           started_at: nonode@nohost
% ok
% 4>




