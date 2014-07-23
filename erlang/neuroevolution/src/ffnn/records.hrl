%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 17:09
%%%------------------------------------------------------------------------------------------------
-author("Temple").

%% API
-export([]).

% *************************************************************************************************
% Denotes 4 genotype record formats for a directly encoded feed forward neural network.
%
% Cortex    : An element type to record, access and manage all elements in the network.
% Sensor    : An element type to initiate a 'pulse' through the network.
% Neuron    : An element type to process specified  input signals and propgate them to specified
%             ouputs.
% Actuator  : An element type to perform an action on the enivornment based on it's input.
%
%

% *************************************************************************************************
% The 'cortex' record is a direct genotype encoding of a 'cortex' entity that maintains references
% to all 'sensor', neuron', and, 'actuator' nodes in the genotype encoding.
%
% It's phenotype is responsible for initialising the network, destroying the networkm and initiating
% an input pulse (input vector) though the network.
%
% id            : {cortex, UniqueId} - The tuple reference id of the cortex.
% sensor_ids    : A list of 'sensor' ids.
% actuator_ids  : A list of 'actuator' ids.
% nids          : A list of 'neuron' ids.
%
-record(cortex, {id, sensor_ids, actuator_ids, nids}).



% *************************************************************************************************
% The 'sensor' record is a direct genotype encoding of a 'sensor element' in a simple feed forward
% neural network.
%
% It's phenotype is responsible for initiating an input pulse (input vector) to the first layer of
% the network from some specified source (camera, microphone, database, etc.).
%
% id          : {sensor, UniqueId} - The tuple reference id of the sensor node.
% cx_id       : A reference to the parent cortex id.
% name        : The name of the phenotypic function executed to generate a signal.
% vl          : The 'vector length' of the 'signal' output vector.
% fanout_ids  : A list of (first layer) 'neuron ids' to propogate the output signal to.
%
-record(sensor, {id, cx_id, name, vl, fanout_ids}).



% *************************************************************************************************
% The 'neuron' record is a direct genotype encoding of a 'neuron element' in a simple feed forward
% neural network.
%
% It's phenotype is responsible for collating an ordered real valued n-input vector from each input,
% calculating a single real 1-ouput vector, and propgating it to the output nodes.
%
% id          : {sensor, UniqueId} - The tuple reference id of the neuron node.
% cx_id       : A reference to the parent cortex id.
% name        : The name of the phenotypic function executed to process a signal.
% af          : The 'activation function' (e.g. 'tanh') applied to the calculated dot product.
% output_ids  : A list of 'neuron ids' / 'actuator ids' to propogate the output signal to.
%
-record(neuron, {id, cx_id, af, input_idps, output_ids}).



% *************************************************************************************************
% The 'actuator' record is a direct genotype encoding of an 'actuator element' in a simple feed
% forward neural network.
%
% It's phenotype is responsible for performing a specified output based on it's input (screen,
% controllers, etc.).
%
% id          : {actuator, UniqueId} - The tuple reference id of the actuator node.
% cx_id       : A reference to the parent cortex id.
% name        : The name of the phenotypic function executed to perform the required action.
% vl          : The 'vector length' of the 'signal' input vector.
% fanin_ids  : A list of (last layer) 'neuron ids' to accept the output signal from.
%
-record(actuator,{id, cx_id, name, vl, fanin_ids}).

