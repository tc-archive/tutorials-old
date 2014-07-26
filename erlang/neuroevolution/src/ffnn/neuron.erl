%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 19:49
%%%------------------------------------------------------------------------------------------------
-module(neuron).
-author("Temple").

%% API
-export([]).
-compile(export_all).
-include("records.hrl").


% *************************************************************************************************
%% Neurone
%%
%% The neuron module is part of the 'exoself' genotype <---> phenotype mapper.
%%
%% The spawned neuron process is responsible for:
%% 1) Creating an neuron process phenotype from its genotype representation.
%% 2) Collating the input results from the previous layer.
%% 3) Calculating the output values.
%% 4) Forwarding the ouput result to all connected neurons in the next layer.
%% 5) Shutting doen the neuron process.
%%
%%

% *************************************************************************************************
%% Usage
%%
%% 1>c(neuron).
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
% When gen/2 is executed it spawns the neuron element and immediately begins to wait for its initial
% state message.
%

%
% --- State Params ---
%
% ExoSelf_PId       : ExoSelf PId
% {TUPLE}
%   Id                : The 'sensor id'.
%   Cx_PId            : The PId of the parent cortex process.
%   AF                : The name of the sigmoidal 'activation' function, e.g. 'tanh'.
%   Input_PIdPs       : The signal input PIds.
%   Output_PIds       : The signal output PIds.
%
loop(ExoSelf_PId) ->
	receive
		% Initialise the sensor process from neuron genotype tuple.
		{ExoSelf_PId,{Id,Cx_PId,AF,Input_PIdPs,Output_PIds}} ->
			% Keep alive.
			loop(Id,Cx_PId,AF,{Input_PIdPs,Input_PIdPs},Output_PIds,0)

	end.


% *************************************************************************************************
% The neuron process waits for vector signals from all the processes that it’s connected from,
% taking the dot product of the input and weight vectors, and then adding it to the accumulator.
% Once all the signals from Input_PIds are received, the accumulator contains the dot product to
% which the neuron then adds the bias and executes the activation function on. After fanning out the
% output signal, the neuron again returns to waiting for incoming signals. When the neuron receives
% the {Cx_PId,get_backup} message, it forwards to the cortex its full MInput_PIdPs list, and its Id.
% Once the training/learning algorithm is added to the system, the MInput_PIdPs would contain a full
% set of the most recent and updated version of the weights.
%

%
% --- State Params ---
%
% Id                : The 'sensor id'.
% Cx_PId            : The PId of the parent cortex process.
% AF                : The name of the sigmoidal 'activation' function, e.g. 'tanh'.
% {[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs}
%                   : Tuple looping {Input_PId,Weights} with original [{Input_PId,Weights}} copy.
% Output_PIds       : A list of output PIds to forward the output to.
% Acc               : An accumulator collating the neural output.
%
loop(Id,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc) ->
	receive

		% Receive a 'forward' message with the specified Input from a forwarding process (Input_PId).
		{Input_PId,forward,Input} ->
			% Calculate the dot prod of the input and weights.
			Result = dot(Input,Weights,0),
			% Add the result to the accumulator (output signal) and process the next (head) Input_PIdPs.
			loop(Id,Cx_PId,AF,{Input_PIdPs,MInput_PIdPs},Output_PIds,Result+Acc);

		% Receive a 'backup' message from the cortext.
		{Cx_PId,get_backup} ->
			% Send the list of MInput_PIdPs to the cortex process.
			Cx_PId ! {self(),Id,MInput_PIdPs},
			% Keep alive!
			loop(Id,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc);

		% Receive a 'terminate' message from the cortext.
		{Cx_PId,terminate} ->
			ok

	end;
% --- State Params ---
%
% Id                : The 'sensor id'.
% Cx_PId            : The PId of the parent cortex process.
% AF                : The name of the sigmoidal 'activation' function, e.g. 'tanh'.
% {[Bias],MInput_PIdPs}
%                   : The final loop processing 'bias' Tuple {Input_PId,Weights} with original
%                     [{Input_PId,Weights}} copy.
% Output_PIds       : A list of output PIds to forward the output to.
% Acc               : An accumulator collating the neural output.
%
loop(Id,Cx_PId,AF,{[Bias],MInput_PIdPs},Output_PIds,Acc) ->
	% Add the bias to the output and apply the activation function.
	Output = neuron:AF(Acc+Bias),
	% For each output process; forward the single Output as a list.
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	% Keep Alive! Reset the Input_PIdPs and Acc.
	loop(Id,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0);
% --- State Params ---
%
% Id                : The 'sensor id'.
% Cx_PId            : The PId of the parent cortex process.
% AF                : The name of the sigmoidal 'activation' function, e.g. 'tanh'.
% {[],MInput_PIdPs}
%                   : The final loop terminating emptyu Tuple {Input_PId,Weights} with original
%                     [{Input_PId,Weights}} copy.
% Output_PIds       : A list of output PIds to forward the output to.
% Acc               : An accumulator collating the neural output.
%
loop(Id,Cx_PId,AF,{[],MInput_PIdPs},Output_PIds,Acc) ->
	% Add the bias to the output and apply the activation function.
	Output = neuron:AF(Acc),
	% For each output process; forward the single Output as a list.
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	% Keep Alive! Reset the Input_PIdPs and Acc.
	loop(Id,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0).


% DOT PRODCT
dot([I|Input],[W|Weights],Acc) ->
	dot(Input,Weights,I*W+Acc);
dot([],[],Acc) ->
	Acc.


% *************************************************************************************************
% Though in this current implementation the neuron has only the tanh/1 function available to it, we
% will later extend the system to allow different neurons to use different activation functions.
%
tanh(Val)-> math:tanh(Val).