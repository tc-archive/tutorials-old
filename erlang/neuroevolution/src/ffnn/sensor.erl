%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 19:33
%%%------------------------------------------------------------------------------------------------
-module(sensor).
-author("Temple").

%% API
-export([]).
-compile(export_all).

-include("records.hrl").


% *************************************************************************************************
%% Sensor
%%
%% The sensor module is part of the 'exoself' genotype <---> phenotype mapper.
%%
%% The spawned sensor process is responsible for:
%% 1) Creating a sensor process phenotype from its genotype representation.
%% 2) Handling 'sync' messages from the parent cortex process to initiate a pulse.
%% 3) Initiating a 'pulses' through the feed forward network by sending a vector input to each
%%    neuron process in the first hidden layer.
%% 4) Shutting doen the sensor process.
%%
%%

% *************************************************************************************************
%% Usage
%%
%% 1>c(sensor).
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
gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).

% *************************************************************************************************
% When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial
% state message.
%
loop(ExoSelf_PId) ->
	receive
		% Initialise the sensor process from sensor genotype tuple.
		{ExoSelf_PId,{Id,Cx_PId,SensorName,VL,Fanout_PIds}} ->
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds) % Keep alive.
	end.


% *************************************************************************************************
% The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either
% be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex
% requests so.
%
% --- State Params ---
%
% Id                : The 'sensor id'.
% Cx_PId            : The PId of the parent cortex process.
% SensorName        : The name of the function to invoke to generate a signal (Currently, only 'rng').
% VL                : The length of the generated sensor vector signal.
% Fanout_PIds       : The neuron Pid list of the first layer.
%
loop(Id,Cx_PId,SensorName,VL,Fanout_PIds) ->
	receive

		% Receive a 'sync' message from the cortex.
		{Cx_PId,sync} ->
			% Invoke the sensor signal generating function.
			SensoryVector = sensor:SensorName(VL),
			% Propogate the signal to all neurons in the first layer.
			[Pid ! {self(),forward,SensoryVector} || Pid <- Fanout_PIds],
			% Keep alive.
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds);

		% Receive a 'terminate' message from the cortex.
		{Cx_PId,terminate} ->
			ok

	end.


% *************************************************************************************************
% ’rng’ is a simple random number generator that produces a vector of random values, each between 0
% and 1. The length of the vector is defined by the VL, which itself is specified within the sensor
% record.
%
rng(VL) ->
	rng(VL,[]).
rng(0,Acc) ->
	Acc;
rng(VL,Acc) ->
	rng(VL-1,[random:uniform()|Acc]).
