%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 19:14
%%%------------------------------------------------------------------------------------------------
-module(cortex).
-author("Temple").

%% API
-export([]).
-compile(export_all).
-include("records.hrl").

% *************************************************************************************************
%% Cortex
%%
%% The cortex module is part of the 'exoself' genotype <---> phenotype mapper.
%%

% *************************************************************************************************
%% Usage
%%
%% 1>c(constructor).
%%


% *************************************************************************************************
% The gen/2 function spawns the cortex element, which immediately starts to wait for a the state
% message from the same process that spawned it, exoself.
%
% The initial state message contains the sensor, actuator, and neuron PId lists. The message also
% specifies how many total  Sense-Think-Act cycles the Cortex should execute before terminating
% the NN system.
%
% Once we  implement the learning algorithm, the termination criteria will depend on the fitness
% of the NN, or some other useful property.
%
%
% --- Erlang ---
%
% A distributed Erlang system consists of a number of Erlang runtime systems communicating with
% each other. Each such runtime system is called a node. Message passing between processes at
% different nodes, as well as links and monitors, are transparent when pids are used. Registered
% names, however, are local to each node. This means the node must be specified as well when
% sending messages etc. using registered names.
%
%
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

loop(ExoSelf_PId) ->

	receive
		% Initialise the Exoself process
		{ExoSelf_PId,{Id,SPIds,APIds,NPIds},TotSteps} ->

			% Send a 'sync' message to each sensor.
			[SPId ! {self(),sync} || SPId <- SPIds],

			% Keep alive, with initialised state. Clone APIds to loop over.
			loop(Id,ExoSelf_PId,SPIds,{APIds,APIds},NPIds,TotSteps)
	end.


% *************************************************************************************************
% The cortex’s goal is to synchronize the NN system such that when the actuators have received all
% their control signals, the sensors are once again triggered to gather new sensory information.
%
% Thus the cortex waits for the sync messages from the actuator PIds in its system, and once it has
% received all the sync messages, it triggers the sensors and then drops back to waiting for a new
% set of sync messages.
%
% The cortex stores 2 copies of the actuator PIds: the APIds, and the MemoryAPIds (MAPIds). Once all
% the actuators have sent it the sync messages, it can restore the APIds list from the MAPIds.
%
% Finally, there is also the Step variable which decrements every time  a full cycle of
% Sense-Think-Act completes, once this reaches 0, the NN system begins its termination and backup
% process.
%
%


% Actuator Monitoring loop.
%
% Monitor each APId process. When its {APId,sync} is recieved removed it from the monitored
% list. When the APIds list is exhausted, all APId process have recieved a 'sync' and a 'pulse'
% through the network is complete.
%
% --- Params ---
%
% Id                    : The 'cortex id'.
% ExoSelf_PId           : The PId of the parent ExoSelf process.
% SPIds                 : A list of 'sensor' process ids.
% {[APId|APIds],MAPIds} : A tuple of the acutator PId [H|T] list, and the (new) memory actuator PId list.
% NPIds                 : The neuron Pid list.
% Step                  : The total step until termination.
%
loop(Id,ExoSelf_PId,SPIds,{[APId|APIds],MAPIds},NPIds,Step) ->
	receive

		% A 'sync' message for the specified APId has been recieved.
		{APId,sync} ->
			% Keep alive and wait for 'sync' messages to unsynced APIds (if required).
			loop(Id,ExoSelf_PId,SPIds,{APIds,MAPIds},NPIds,Step);

		terminate ->
			% 'terminate'
			io:format("Cortex:~p is terminating.~n",[Id]),
			% Send a 'terminate' message to all processes.
			[PId ! {self(),terminate} || PId <- SPIds],
			[PId ! {self(),terminate} || PId <- MAPIds], [PId ! {self(),termiante} || PId <- NPIds]

	end;
% NW Pulse Complete loop - (APIds = [])
%
% We have fully processed  a pulse through the network. All actuators exhausted (have recieved
% 'sync' messages).
%
% --- Params ---
%
% Id                : The 'cortex id'.
% ExoSelf_PId       : The PId of the parent ExoSelf process.
% SPIds             : A list of 'sensor' process ids.
% {[],MAPIds}       : A tuple of the unsynced APId list(empty), and the (original) APId list.
% NPIds             : The neuron Pid list.
% Step == 0         : The total step until termination.
%
loop(Id,ExoSelf_PId,SPIds,{[],MAPIds},NPIds,Step) ->

	% Send a 'message' to each 'sensor process' to initiate a new pulse through the system.
	[PId ! {self(),sync} || PId <- SPIds],
	% Keep alive! and ...
	% Reset the APId list (from the stored MAPId list);
	% Decrement the 'step' (termination) counter.
	loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,Step-1);
% Termination loop - (Step = 0)
%
% We have processed that many initiating 'sync' requests / 'pulses' through the network.
%
% NB: Once we  implement the learning algorithm, the termination criteria will depend on the
% fitness of the NN, or some other useful property.
%
% --- Params ---
%
% Id                : The 'cortex id'.
% ExoSelf_PId       : The PId of the parent ExoSelf process.
% SPIds             : A list of 'sensor' process ids.
% {_APIds,MAPIds}   : A tuple of the acutator PId list, and the (new) memory actuator PId list.
% NPIds             : The neuron Pid list.
% Step == 0         : The total step until termination. Here at 0 the process backs-up and terminates.
%
loop(Id,ExoSelf_PId,SPIds,{_APIds,MAPIds},NPIds,0) ->

	io:format("Cortex:~p is backing up and terminating.~n",[Id]),

	% Get a list of the {NId,WeightTuples} tuples.
	Neuron_IdsNWeights = get_backup(NPIds,[]),

	% Send a 'backup' message to the ExoSef process.
	ExoSelf_PId ! {self(),backup,Neuron_IdsNWeights},

	% Send a 'terminate' message to all processes.
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- MAPIds],
	[PId ! {self(),termiante} || PId <- NPIds].


% *************************************************************************************************
% During backup, cortex contacts all the neurons in its NN and requests for the neuron’s Ids and
% their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, the list
% is sent to exoself for the actual backup and storage.
%
get_backup([NPId|NPIds],Acc) ->
	NPId ! {self(),get_backup},
	receive
		{NPId,NId,WeightTuples} ->
			get_backup(NPIds,[{NId,WeightTuples}|Acc])
	end;

get_backup([],Acc)->
		Acc.
