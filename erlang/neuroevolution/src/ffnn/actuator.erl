%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 19:41
%%%------------------------------------------------------------------------------------------------
-module(actuator).
-author("Temple").

%% API
-export([]).

-compile(export_all).
-include("records.hrl").


% *************************************************************************************************
%% Actuator
%%
%% The sensor module is part of the 'exoself' genotype <---> phenotype mapper.
%%
%% The spawned actuator process is responsible for:
%%
%%

% *************************************************************************************************
%% Usage
%%
%% 1>c(actuator).
%%


% --- Params ---
%
% ExoSelf_PId     : The PId of the ExoSelf process,
% Node            : The Erlang 'Node'
%
% --- Return ---
%
% Returns the pid of a new process started by the application of Module:Function to Args on Node.
% If Node does not exists, a useless pid is returned.
%
gen(ExoSelf_PId,Node) ->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).


% *************************************************************************************************
% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its
% initial state message.
%
loop(ExoSelf_PId) ->
	receive
		% Initialise the actuator process from actuator genotype tuple.
		{ExoSelf_PId,{Id,Cx_PId,ActuatorName,Fanin_PIds}} ->
			loop(Id,Cx_PId,ActuatorName,{Fanin_PIds,Fanin_PIds},[])
	end.


% *************************************************************************************************
% The actuator process gathers the control signals from the neurons, appending them to the
% accumulator. The order in which the signals are accumulated into a vector is in the same order as
% the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends
% cortex the sync signal, executes its function, and then again begins to wait for the neural signals
% from the output layer by reseting the Fanin_PIds from the second copy of the list.
%

% --- State Params ---
%
% Id                : The 'actuator id'.
% Cx_PId            : The PId of the parent cortex process.
% {[From_PId|Fanin_PIds],MFanin_PIds}
%                   : Input PId looping tuple.
% Acc               : The accumulator.
%
loop(Id,Cx_PId,AName,{[From_PId|Fanin_PIds],MFanin_PIds},Acc) ->
	receive

	% Receive a 'forward' message with the specified Input from a forwarding process.
	{From_PId,forward,Input} ->
		loop(Id,Cx_PId,AName,{Fanin_PIds,MFanin_PIds},  % Ready to process next element.
		lists:append(Input,Acc));                       % Append the input to the result vector.

	% Receive a 'terminate' message the cortex.
	{Cx_PId,terminate} ->
		ok

	end;
% --- State Params ---
%
% Id                : The 'actuator id'.
% Cx_PId            : The PId of the parent cortex process.
% {[],MFanin_PIds}  : Input PId looping tuple. Base Case. Input exhausted.
% Acc               : The length of the generated sensor vector signal.
%
loop(Id,Cx_PId,AName,{[],MFanin_PIds},Acc) ->

	% Invoke the actuator environment acting function with the colated ouput vector..
	actuator:AName(lists:reverse(Acc)),                 % Re-order the result vector.

	% Send a sync message to the cortex process.
	Cx_PId ! {self(),sync},

	% Keep alive! and reinitialise for next pulse...
	% Reset the MFanin_PIds list (from the stored MFanin_PIds list);
	% Re-initialise the Accumulator
	loop(Id,Cx_PId,AName,{MFanin_PIds,MFanin_PIds},[]).


% *************************************************************************************************
% The pts actuation function simply prints to screen the vector passed to it.
pts(Result)->
	io:format("actuator:pts(Result): ~p~n",[Result]).

