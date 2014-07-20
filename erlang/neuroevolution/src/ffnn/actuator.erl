%%%-------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erland", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 19:41
%%%-------------------------------------------------------------------
-module(actuator).
-author("Temple").

%% API
-export([]).

-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node) ->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).


% *************************************************************************************************
% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its
% initial state message.
%
loop(ExoSelf_PId) ->
	receive
		{ExoSelf_PId,{Id,Cx_PId,ActuatorName,Fanin_PIds}} ->
			loop(Id,Cx_PId,ActuatorName,{Fanin_PIds,Fanin_PIds},[])
	end.

loop(Id,Cx_PId,AName,{[From_PId|Fanin_PIds],MFanin_PIds},Acc) ->
	receive
	{From_PId,forward,Input} ->
		loop(Id,Cx_PId,AName,{Fanin_PIds,MFanin_PIds},lists:append(Input,Acc));
	{Cx_PId,terminate} ->
		ok
	end;


% *************************************************************************************************
% The actuator process gathers the control signals from the neurons, appending them to the
% accumulator. The order in which the signals are accumulated into a vector is in the same order as
% the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends
% cortex the sync signal, executes its function, and then again begins to wait for the neural signals
% from the output layer by reseting the Fanin_PIds from the second copy of the list.
%
loop(Id,Cx_PId,AName,{[],MFanin_PIds},Acc) ->
	actuator:AName(lists:reverse(Acc)),
	Cx_PId ! {self(),sync},
	loop(Id,Cx_PId,AName,{MFanin_PIds,MFanin_PIds},[]).

% *************************************************************************************************
% The pts actuation function simply prints to screen the vector passed to it.
pts(Result)->
	io:format("actuator:pts(Result): ~p~n",[Result]).

