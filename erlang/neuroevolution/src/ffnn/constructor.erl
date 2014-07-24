%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 17:12
%%%------------------------------------------------------------------------------------------------
-module(constructor).
-author("Temple").

%% API
-export([]).
-compile(export_all).
-include("records.hrl").


% *************************************************************************************************
%% Usage
%%
%% 1>c(constructor).
%% {ok,constructor}.
%% 2>constructor:construct_Genotype(ffnn,rng,pts,[1,3]).
%% ok
%%
%%
%% Open the file to which the Genotype was written (ffnn in the above example), and peruse the
%% generated list of tuples to ensure that all the elements are properly interconnected by looking at
%% their fanin/fanout and input/output ids. This list is a genotype of the NN which is composed of 3
%% feed forward neural layers, with 1 neuron in the first layer, 3 in the second, and 1 in the third.
%% The created NN genotype uses the rng sensor and pts actuator.
%%



% *************************************************************************************************
% The construct_Genotype function accepts the name of the file to which we’ll save the 'genotype',
% 'sensor name', 'actuator name', and the 'hidden layer density' parameters.
%
% We have to generate unique Ids for every sensor and actuator. The sensor and actuator names are
% used  as input to the create_Sensor and create_Actuator functions, which in turn generate the
% actual Sensor and Actuator representing tuples. We create unique Ids for sensors and actuators so
% that when in the future a NN uses 2 or more sensors or actuators of the same type, we will be
% able to differentiate between them using their Ids.
%
% After the Sensor and Actuator tuples are generated, we extract the NN’s input and output vector
% lengths from the sensor and actuator used by the system. The Input_VL is then used to specify how
% many weights the neurons in the input layer will need, and the Output_VL specifies how many
% neurons  are in the output layer of the NN.
%
% After appending the HiddenLayerDensites to the now known number of neurons in the last layer to
% generate the full LayerDensities list, we use the create_NeuroLayers function to generate the
% Neuron representing tuples.
%
% In this way, the hidden layer density vector list denotes the number neurones in each layer
% exceot for the final layer which is determines by the size of the vector accepted by the
% terminating actuator. For example:
%
% HLD: [] - Produces a single (output) layer one neuron network.
%
%           [S] -------------------------> [N] ----> [A]
%
% HLD: [1] - Produces a two layer (one hidden) two neuron network.
%
%           [S] ----> [N] ---------------> [N] ----> [A]
%
% HLD: [2] - Produces a two layer (one hidden) three neuron network.
%
%                /--> [N] -->\
%           [S] +             + ---------- [N] ----> [A]
%                \--> [N] -->/
%
% HLD: [1, 2] - Produces a three layer (2 hidden) four neuron network.
%
%                          /--> [N] --->\
%           [S] ----> [N] +              + [N] ----> [A]
%                          \--> [N] --->/
%
%
% We then update the Sensor and Actuator records with proper fanin and fanout ids from the
% freshly created Neuron tuples, compose the Cortex, and write the genotype to file.
%
%
% --- Params ---
%
% FileName              : The name of the output file to which the genotype will be output.
% SensorName            : The sensor to use for the single sensor.
%                         NB: The Input_VL of the sensor used to specify how  many weights
%                             the neurons in the input layer will need.
% ActuatorName          : The actuator function atom to use for the single actuator.
%                         NB: Output_VL specifies how many neurons  are in the output layer
%                             of the NN.
% HiddenLayerDensities  : The required layer densities of the hidden layer.
%
construct_Genotype(FileName,SensorName,ActuatorName,HiddenLayerDensities) ->

	% Generate 'cortex id'.
	Cx_Id = {cortex,generate_id()},

	% Create 'sensor record' and 'actuator record'
	S = create_Sensor(SensorName),        % Currently only 'rng' is supported.
	A = create_Actuator(ActuatorName),    % Currently only 'pts' is supportd.

	% Determine the 'LayerDensity' list from the 'HiddenLayerDensities' list and the 'Output_VL'
	% of the  actuator record.
	Output_VL = A#actuator.vl,
	LayerDensities = lists:append(HiddenLayerDensities,[Output_VL]),

	% Create a list of 'Neural Layers'. Each element in the list is another list where each
	% element is a 'neuron' record.
	Neurons = create_NeuroLayers(Cx_Id,S,A,LayerDensities),

	% Extract the input 'nids' of the neuron records in the first layer.
	[Input_Layer|_] = Neurons,
	FL_NIds = [N#neuron.id || N <- Input_Layer],

	% Extract the output 'nids' of the neuron records in the last layer.
	[Output_Layer|_] = lists:reverse(Neurons),
	LL_NIds = [N#neuron.id || N <- Output_Layer],

	% Extract the unique set of 'nids' of all neurons in all layers.
	NIds = [N#neuron.id || N <- lists:flatten(Neurons)],

	% Create the 'sensor', 'actuator' and 'cortex' records.
	Sensor = S#sensor{cx_id = Cx_Id, fanout_ids = FL_NIds},
	Actuator = A#actuator{cx_id=Cx_Id,fanin_ids = LL_NIds},
	Cortex = create_Cortex(Cx_Id,[S#sensor.id],[A#actuator.id],NIds),

	% Build the Genotype. A list of all the cortex, sensor, actuator, and neuron records.
	Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]),

	% Write the Genotype to the specified file.
	{ok, File} = file:open(FileName, write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Genotype),
	file:close(File).


construct_Genotype(SensorName,ActuatorName,HiddenLayerDensities) ->

	construct_Genotype(ffnn,SensorName,ActuatorName,HiddenLayerDensities).


% *************************************************************************************************
% The create_Cortex/4 function generates the record encoded genotypical representation of the
% cortex element. The Cortex element needs to know the Id of every Neuron, Sensor, and Actuator
% in the NN.
%
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% S_Ids      : A list of [{input_id, input_vector_size},...] tuples.
% A_Ids              : A exhausted list of [{neuron, {layer_idx, id}},...] tuples.
% NIds      : The required layer densities of the network to be generated.
%
% --- Return ---
%
% A 'cortex' record linking together all records in the netowrk by ''Id'.
%
create_Cortex(Cx_Id,S_Ids,A_Ids,NIds) ->
	#cortex{id = Cx_Id, sensor_ids=S_Ids, actuator_ids=A_Ids, nids = NIds}.



% *************************************************************************************************
% Every sensor and actuator uses some kind of function associated with it, a function that either
% polls the environment for sensory signals (in the case of a sensor) or acts upon the environment
% (in the case of an actuator).
%
% It is the function that we need to define and program before it is used, and the name of the
% function is the same as the name of the sensor or actuator itself.
%
% For example, the create_Sensor/1 has specified only the rng sensor, because that is the only
% sensor function we’ve finished developing. The rng function has its own vl specification, which
% will determine the number of weights that a neuron will need to allocate if it is to accept this
% sensor’s output vector.
%
% The same principles apply to the create_Actuator function.
%
% Both, create_Sensor and create_Actuator function, given the name of the sensor or actuator, will
% return a record with all the specifications of that element, each with its own unique Id.
%
create_Sensor(SensorName) ->

	case SensorName of

		rng ->
			#sensor{id={sensor,generate_id()},name=rng,vl=2};     % Create the 'rng' sensor record.

		_ ->
			exit("System does not yet support a sensor by the name:~p.",[SensorName])

	end.

create_Actuator(ActuatorName) ->

	case ActuatorName of

		pts ->
			#actuator{id={actuator,generate_id()},name=pts,vl=1}; % Create the 'pts' actuator record.

	  _ ->
			exit("System does not yet support an actuator by the name:~p.",[ActuatorName])

	end.



% *************************************************************************************************
% The function create_NeuroLayers/3 prepares the initial step before starting the recursive
% create_NeuroLayers/7 function which will create all the Neuron records.
%
% We first generate the place holder Input Ids “Plus”(Input_IdPs), which are tuples composed of Ids
% and the vector lengths of the incoming signals associated with them. The proper input_idps will
% have a weight list in the tuple instead of the vector length. Because we are only building NNs
% each with only a single Sensor and Actuator, the IdP to the first layer is composed of the single
% Sensor Id with the vector length of its sensory signal, likewise in the case of the Actuator.
%
% We then generate unique ids for the neurons in the first layer, and drop into the recursive
% create_NeuroLayers/7 function.
%
%
% --- Erlang ---
%
% This method utiises Erlang 'list comprehensions'.
%
% See: http://www.erlang.org/doc/programming_examples/list_comprehensions.html
% See: http://stackoverflow.com/questions/1570100/need-help-understanding-this-erlang-code
%
% 1) [something(X) || X <- L],
% - A list comprehension. L is a list of elements, and this expression creates a list of new
%   elements, forming each element by invoking something() on it.
%
% 2) [something(X,Y) || X <-L, Y<-M]
% - A list comprehension for the Cartesian product of each element in X and Y.
%
% 3) [something(X) || X <-L, Expr]
% - A filter expression. Same as the first one, but it is executed only for elements of L, where
%   Expr is true for the given X.
%
% 4) [something(X) || {X,..} <-L, Expr]
% - Another kind of filter. In the list comprehension only those elements are taken that can be
%   matched by the element.
%
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% Sensor          : The sensor record.
% Actuator        : The actuator record.
% LayerDensities  : The required layer densities of the network to be generated.
%
% --- Return ---
%
% An ordered List of Lists. Where each List is a 'layer' in the NN genotype encoding; and each
% element in each layer is a sensor, neuron, or actuator record. The first element is the single
% 'sensor record'; and the last the 'actuator  record'.
%
create_NeuroLayers(Cx_Id,Sensor,Actuator,LayerDensities) ->

	% Generates a list of {neuron, {layer_idx, id}} 'neuron id' tuples for each neuron in the
	% first (layer_idx = 1) layer.
	%
	% Uses a list comprehension with a generator containing the number of neurons in the
	% first layer, and an epty list as the accunulator.
	[FL_Neurons|Next_LDs] = LayerDensities,
	NIds = [{neuron,{1,Id}} || Id <- generate_ids(FL_Neurons,[])],

	% Recursively create a list
	Input_IdPs = [{Sensor#sensor.id,Sensor#sensor.vl}], % A list of [{input_id, input_vector_size}]
																											% elements (for sensor).
	Tot_Layers = length(LayerDensities),
	create_NeuroLayers(Cx_Id,Actuator#actuator.id,1,Tot_Layers,Input_IdPs,NIds,Next_LDs,[]).


% *************************************************************************************************
% During the first iteration, the first layer neuron ids constructed in create_NeuroLayers/3 are
% held in the NIds variable.
%
% In create_NeuroLayers/7, with every iteration we generate the Output_NIds, which are the Ids of
% the neurons in the next layer.
%
% The last layer is a special case which occurs when LayerIndex == Tot_Layers.
%
% Having the Input_IdPs, and the Output_NIds, we are able to construct a neuron record for every Id
% in NIds using the function create_layer/4. The Ids of the constructed Output_NIds will become the
% NIds variable of the next iteration, and the Ids of the neurons in the current layer will be
% extended and become Next_InputIdPs. We then drop into the next iteration with the newly prepared
% Next_InputIdPs and Output_NIds.
%
% Finally, when we reach the last layer, the Output_Ids is the list containing a single Id of the
% Actuator element. We use the same function, create_NeuroLayer/4, to construct the last layer and
% return the result.
%
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% Actuator_Id     : The id of the actuator element.
% LayerIndex      : The integer index of the layer being computed for this invocation.
% Tot_Layers      : The integer total number of layers being generated.
% Input_IdPs      : A list of [{input_id, input_vector_size},...] tuples.
% NIds            : A list of [{neuron, {layer_idx, id},...] tuples.
% [Next_LD|LDs]   : A list of [ld_x,...] 'layer density integers.
% Acc             : An accumulator list for the 'Layer_Neurons' Lists being generated.
%
% --- Return ---
%
% An ordered List of Lists. Where each List is a 'layer' in the NN genotype encoding; and each
% element in each layer is a sensor, neuron, or actuator record. The first element is the single
% 'sensor record'; and the last the 'actuator  record'.
%
create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex,Tot_Layers,Input_IdPs,NIds,[Next_LD|LDs], Acc) ->

	% Generate the 'neuron_id' tuples for the next layer.
	Output_NIds = [{neuron,{LayerIndex+1,Id}} || Id <- generate_ids(Next_LD,[])],

	% Main - Generate the 'neuron records' for this layer.
	Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_NIds,[]),

	% Generate the next layers list of [{input_id, input_vector_size},...] tuples.
	% As we are in a 'neural layer' (not sensor or actuator) the 'input_vector-size' is 1.
	Next_InputIdPs = [{NId,1}|| NId <- NIds],

	% Recursively call 'create_NeuroLayers' to build the next layer untl the base case is reached.
	create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex+1,Tot_Layers,Next_InputIdPs,Output_NIds, LDs,[Layer_Neurons|Acc]);


% *************************************************************************************************
% Base Case: (LayerIndex == Tot_Layers) && ([Next_LD|LDs] == [])
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% Actuator_Id     : The id of the actuator element.
% Tot_Layers      : The integer total number of layers being generated.
% Tot_Layers      : The integer total number of layers being generated.
% Input_IdPs      : A list of [{input_id, input_vector_size},...] tuples.
% NIds            : A list of [{neuron, {layer_idx, id}},...] tuples.
% []              : An empty list of [ld_x,...] 'layer density integers.
% Acc             : An accumulator list for the 'Layer_Neurons' Lists being generated.
%
% --- Return ---
%
% An ordered List of Lists. Where each List is a 'layer' in the NN genotype encoding; and each
% element in each layer is a sensor, neuron, or actuator record. The first element is the single
% 'sensor record'; and the last the 'actuator  record'.
%
create_NeuroLayers(Cx_Id,Actuator_Id,Tot_Layers,Tot_Layers,Input_IdPs,NIds,[],Acc) ->

	% In the final layer there is a single actuator record,
	Output_Ids = [Actuator_Id],

	% Main - Generate the 'neuron records' for this layer.
	Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[]),

	% Add the final layer and reverse the accumlated layer list to order it correctly.
	% The sensor layer should be first; then each neural layer, and the actuator layer should be
	% last.
	lists:reverse([Layer_Neurons|Acc]).



% *************************************************************************************************
% To create neurons from the same layer, all that is needed are the Ids for those neurons, a list
% of Input_IdPs for every neuron so that we can create the proper number of weights, and a list of
% Output_Ids.
%
% Since in our simple feed forward neural network all neurons are fully connected to the neurons
% in the next layer, the Input_IdPs and Output_Ids are the same for every neuron belonging to the
% same layer.
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% Input_IdPs      : A list of [{input_id, input_vector_size},...] tuples.
% [Id|NIds]       : A list of [{neuron, {layer_idx, id}},...] tuples.
% Output_Ids      : The list of output (neuron / actuator) records.
% Acc             : The accumlator list. Accumulating neuron records for this network layer.
%
% --- Return ---
%
% An ordered List of neuron records for this network layer.
%
create_NeuroLayer(Cx_Id,Input_IdPs,[Id|NIds],Output_Ids,Acc) ->

	% Create a 'neuron record' for this 'neuron id'.
	Neuron = create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids),

	% Add the record to the result list; and process the next 'NId'.
	create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[Neuron|Acc]);


% Base case. There are no more NIds to add to this layer. Return the accumlated result.
%
% --- Params ---
%
% Cx_Id           : The id of the cortex element.
% Input_IdPs      : A list of [{input_id, input_vector_size},...] tuples.
% []              : A exhausted list of [{neuron, {layer_idx, id}},...] tuples.
% Output_Ids      : The required layer densities of the network to be generated.
% Acc             : The accumlator list. Accumulating neuron records for this network layer.
%
% --- Return ---
%
% An ordered List of neuron records for this network layer.
%
create_NeuroLayer(_Cx_Id,_Input_IdPs,[],_Output_Ids,Acc) ->
	Acc.


% *************************************************************************************************
% Each neuron record is composed by the create_Neuron/3 function. The create_Neuron/3 function
% creates the Input list from the tuples [{Id,Weights}...] using the vector lengths specified in the
% place holder Input_IdPs.
%
% The create_NeuralInput/2 function uses create_NeuralWeights/2 to generate the random weights in
% the range of -0.5 to 0.5, adding the bias to the end of the list.
%
% --- Params ---
%
% Input_IdPs      : A list of [{input_id, input_vector_size},...] tuples.
% Cx_Id           : The id of the cortex element.
% Output_Ids      : The required layer densities of the network to be generated.
%
% --- Return ---
%
% An new 'neuron record' with inputs, input weights, activatin function, and ouputs.
%
create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids) ->
	Proper_InputIdPs = create_NeuralInput(Input_IdPs,[]),
	#neuron{id=Id,cx_id = Cx_Id,af=tanh,input_idps=Proper_InputIdPs,output_ids=Output_Ids}.


% Generate an {Input_Id,Weights} tuples list.
%
% --- Params ---
%
% [{Input_Id,Input_VL}|Input_IdPs]      : A list of [{input_id, input_vector_length},...] tuples.
% Acc                                   : The id of the cortex element.
%
% --- Return ---
%
% A list of {Input_Id,Weights} tuples. Where InputId is the id of the input source; and
% Weights is a list of randomised floats (for the required input_vector_length), plus, a
% randomised floast 'bias'.
%
create_NeuralInput([{Input_Id,Input_VL}|Input_IdPs],Acc) ->
	% Create a random list of float weights for each input.
	Weights = create_NeuralWeights(Input_VL,[]),
	create_NeuralInput(Input_IdPs,[{Input_Id,Weights}|Acc]);
% Base case. Add bias elements and reverse.
create_NeuralInput([],Acc) ->
	lists:reverse([{bias,random:uniform()-0.5}|Acc]).

% Add a new random float weight to the List.
create_NeuralWeights(Index,Acc) ->
	W = random:uniform()-0.5, create_NeuralWeights(Index-1,[W|Acc]).
% Base case. Return the accumlated list of input float weights.
create_NeuralWeights(0,Acc) ->
	Acc;


% *************************************************************************************************
% The generate_id/0 creates a unique Id using current time, the Id is a floating point value. The
% generate_ids/2 function creates a list of unique Ids.
%
generate_ids(0,Acc) ->
	Acc;
generate_ids(Index,Acc) ->
	Id = generate_id(),
	generate_ids(Index-1,[Id|Acc]).
generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(),
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).


