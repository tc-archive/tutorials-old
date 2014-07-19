%%%-------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jul 2014 19:33
%%%-------------------------------------------------------------------
-module(sensor).
-author("Temple").

%% API
-export([]).
-compile(export_all).

-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).

% *************************************************************************************************
% When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial
% state message.
%
loop(ExoSelf_PId) ->
	receive

		{ExoSelf_PId,{Id,Cx_PId,SensorName,VL,Fanout_PIds}} ->
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds)

	end.


% *************************************************************************************************
% The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either
% be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex
% requests so.
%
loop(Id,Cx_PId,SensorName,VL,Fanout_PIds) ->
	receive

		{Cx_PId,sync} ->
			SensoryVector = sensor:SensorName(VL),
			[Pid ! {self(),forward,SensoryVector} || Pid <- Fanout_PIds],
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds);

		{Cx_PId,terminate} ->
			ok

	end.


% ’rng’ is a simple random number generator that produces a vector of random values, each be- tween 0
% and 1. The length of the vector is defined by the VL, which itself is specified within the sensor
% record.
%
rng(VL) ->
	rng(VL,[]).
rng(0,Acc) ->
	Acc;
rng(VL,Acc) ->
	rng(VL-1,[random:uniform()|Acc]).
