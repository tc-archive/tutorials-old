%%%============================================================================
%%% @doc 
%%% A non OTP version of the 'die_please' OTP GenServer.
%%% @end
%%%============================================================================
-module(die_please_non_otp).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([go/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SLEEP_TIME, 2000).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

go() ->
  %% just sleep for a while, then crash
  timer:sleep(?SLEEP_TIME),
  i_really_want_to_die = right_now.



% 9> die_please_non_otp:go.
% * 1: illegal expression
% 10> die_please_non_otp:go().
% ** exception error: no match of right hand side value right_now
%      in function  die_please_non_otp:go/0 (die_please_non_otp.erl, line 30)
% 11>