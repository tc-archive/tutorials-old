%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 12. Jul 2014 20:01
%%%------------------------------------------------------------------------------------------------
-module(exoself).
-author("Temple").

%% API
-export([]).
-compile(export_all).
-include("records.hrl").


% *************************************************************************************************
%% Exoself
%%
%% The 'exoself' genotype <---> phenotype mapper.
%%
%% Now we create the exoself module, which will map the genotype to phenotype, spawning all the
%% appropriate processes. The exoself module will also provide the algorithm for the Cortex element
%% to update the genotype with the newly trained weights from the phenotype, and in this manner
%% saving the trained and learned NNs for future use.
%%
%%
%% The spawned exoself process is responsible for:
%% 1)
%%
%%

% *************************************************************************************************
%% Usage
%%
%% 1>c(exoself).
%% 2> exoself:map(ffnn).
%%


% Spawn an exoself process Genotype <---> Phenotype mapper from the ff
map()-> map(ffnn).

% Spawn an exoself process Genotype <---> Phenotype mapper, form the specified file.
map(FileName) ->
	% Read the Genotype from the specified file.
	% 'consult' Reads Erlang terms, separated by '.', from the specified file.
	{ok,Genotype} = file:consult(FileName),
	% Spawn a process that maps the genotype into a set of runtime phenotype processes.
	spawn(exoself,map,[FileName,Genotype]).


% *************************************************************************************************
% The map/1 function maps the tuple encoded genotype into a process based phenotype.
%
% The map function expects for the Cx record to be the leading tuple in the tuple list it reads
% from the FileName.
%
% We create an ets table to map Ids to PIds and back again.
%
% Since the Cortex element contains all the Sensor, Actuator, and Neuron Ids, we are able to spawn
% each neuron using its own gen function, and in the process construct a map from Ids to PIds.
%
% We then use link_CerebralUnits to link all non Cortex elements to each other by sending each spawned
% process the information contained in its record, but with Ids converted to Pids where appropriate.
%
% Finally, we provide the  Cortex process with all the PIds in the NN system by executing the
% link_Cortex/2 function.
%
% Once the NN is up and running, exoself starts its wait until the NN has finished its job and is
% ready to backup.
%
% When the cortex initiates the backup process it sends exoself the updated Input_PIdPs from
% its neurons. Exoself uses the update_genotype/3 function to update the old genotype with new
% weights, and then stores the updated version back to its file.
%
% --- Params ---
%
% FileName     : The name of the input/ouput persistent Genotype file.
% Genotype     : The Genotype tuples read from the input file.
%
map(FileName,Genotype) ->

	% Create a new ETS table
	IdsNPIds = ets:new(idsNpids,[set,private]),

	% Extract tuples
	[Cx|CerebralUnits] = Genotype,          % Extract 'cortex' genotype.
	Sensor_Ids = Cx#cortex.sensor_ids,      % Extract 'sensor' id list.
	Actuator_Ids = Cx#cortex.actuator_ids,  % Extract 'actuator' id list.
	NIds = Cx#cortex.nids,                  % Extract 'neuron' id list.

	% Spawn the phenotype processes from the genotype; and map the identifiers.
	spawn_CerebralUnits(IdsNPIds,cortex,[Cx#cortex.id]),    % 'cortex' process
	spawn_CerebralUnits(IdsNPIds,sensor,Sensor_Ids),        % 'sensor' processes
	spawn_CerebralUnits(IdsNPIds,actuator,Actuator_Ids),    % 'actuator' processes
	spawn_CerebralUnits(IdsNPIds,neuron,NIds),              % 'neuron' processes

	% Link the neural processes
	link_CerebralUnits(CerebralUnits,IdsNPIds),
	link_Cortex(Cx,IdsNPIds),

	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),

	receive
		% A 'backup' message from the cortex requesting the persistence of the Neuron_IdsNWeights.
	  % ([{N_Id,PIdPs}..]).
		{Cx_PId,backup,Neuron_IdsNWeights} ->

			% Update the Genotype file with the new neural weights.
			U_Genotype = update_genotype(IdsNPIds,Genotype,Neuron_IdsNWeights),

			% Open and write to file.
			{ok, File} = file:open(FileName, write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, U_Genotype),
			file:close(File),
			io:format("Finished updating to file:~p~n",[FileName])
	end.



% *************************************************************************************************
% We spawn the process for each element based on its type: CerebralUnitType, and the gen function
% that belongs to the CerebralUnitType module.
%
% We then enter the {Id,PId} tuple into our ETS table for later use.
%

% --- Params ---
%
% IdsNPIds             : The ETS table managing the genotpye/phenotype id mapper.
% CerebralUnitType     : Type atom: [cortex|sensor|actuator|neuron].
% [Id|Ids]             : The Genotype tuples read from the input file.
%
spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids]) ->

	% Generate/Spawn a new 'phenotype process'.
	% NB: Uses the 'CerebralUnitType' atom to dynamically invoke the correct module.
	PId = CerebralUnitType:gen(self(),node()),

	ets:insert(IdsNPIds,{Id,PId}),  % Add a GenotypeId --> PhenotypeId mapping to the ETS table.
	ets:insert(IdsNPIds,{PId,Id}),  % Add a PhenotypeId --> GenotypeId mapping to the ETS table.

	% Process remaining Ids...
	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids);
% Recursive base case. All done 'ok'.
spawn_CerebralUnits(_IdsNPIds,_CerebralUnitType,[]) ->
	true.


% *************************************************************************************************
% The link_CerebralUnits/2 converts the Ids to PIds using the created IdsNPids ETS table.
%
% At this point all the elements are spawned, and the processes are waiting for their initial states.
%
% convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that
% the Neuron will know which weights are to be associated with which incoming vector signals.
%
% The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list
% is reversed to take its proper order.
%


% Processes and initialise the sensor process phenotype from the sensor genotype tuples.
%
% --- Params ---
%
% [R|Records]       : A list of 'sensor' record
% IdsNPIds          : The ETS table managing the genotype/phenotype id mapper.
%
link_CerebralUnits([R|Records],IdsNPIds) when is_record(R,sensor) ->

	% Extract the values from the sensor record.
	SId = R#sensor.id,
	SName = R#sensor.name,
	Fanout_Ids = R#sensor.fanout_ids,

	% Look-up and extract the specified elements from the ETS table.
	SPId = ets:lookup_element(IdsNPIds,SId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#sensor.cx_id,2),
	Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],

	% Send the specified extracted genotype record values to initialise the spawned SPId process.
	SPId ! {self(),{SId,Cx_PId,SName,R#sensor.vl,Fanout_PIds}},

	% Process the remaining records.
	link_CerebralUnits(Records,IdsNPIds);


% Processes and initialise the actuator process phenotype from the actuator genotype tuples.
%
% --- Params ---
%
% [R|Records]       : A list of 'actuator' record
% IdsNPIds          : The ETS table managing the genotype/phenotype id mapper.
%
link_CerebralUnits([R|Records],IdsNPIds) when is_record(R,actuator) ->

	% Extract the values from the actuator record.
	AId = R#actuator.id,
	Fanin_Ids = R#actuator.fanin_ids,

	% Look-up and extract the specified elements from the ETS table.
	APId = ets:lookup_element(IdsNPIds,AId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#actuator.cx_id,2), AName = R#actuator.name,
	Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],

	% Send the specified extracted genotype record values to initialise the spawned APId process.
	APId ! {self(),{AId,Cx_PId,AName,Fanin_PIds}},

	% Process the remaining records.
	link_CerebralUnits(Records,IdsNPIds);

% Processes and initialise the neuron process phenotype from the actuator genotype tuples.
%
% --- Params ---
%
% [R|Records]       : A list of 'actuator' record
% IdsNPIds          : The ETS table managing the genotype/phenotype id mapper.
%
link_CerebralUnits([R|Records],IdsNPIds) when is_record(R,neuron) ->

	% Extract the values from the neuron record.
	NId = R#neuron.id,
	AFName = R#neuron.af,
	Input_IdPs = R#neuron.input_idps,
	Output_Ids = R#neuron.output_ids,

	% Look-up and extract the specified elements from the ETS table.
	NPId = ets:lookup_element(IdsNPIds,NId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#neuron.cx_id,2),
	Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]), % Convert GenotypeId to PId PhenotypeId.
	Output_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],

	% Send the specified extracted genotype record values to initialise the spawned NPId process.
	NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds}},

	% Process the remaining records.
	link_CerebralUnits(Records,IdsNPIds);

% Base case. End linking process. Return ok.
link_CerebralUnits([],_IdsNPIds) ->
	ok.


% *************************************************************************************************
% convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that
% the Neuron will know which weights are to be associated with which incoming vector signals.
%

% Base case. Add 'bias' to acumulated vector weight list; and reverse to preserve ordering.
convert_IdPs2PIdPs(_IdsNPIds,[{bias,Bias}],Acc) ->
	lists:reverse([Bias|Acc]);
% --- Params ---
%
% IdsNPIds                  : The ETS table managing the genotype/phenotype id mapper.
% [{Id,Weights}|Fanin_IdPs] : The {Id,Weights}
% Acc                       : Looping output list accumulator.
%
convert_IdPs2PIdPs(IdsNPIds,[{Id,Weights}|Fanin_IdPs],Acc) ->
	% Loop process the next element.
	convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),Weights}|Acc]).



% *************************************************************************************************
% The cortex is initialized to its proper state just as other elements.
%
% Because we have not yet implemented a learning algorithm for our NN system, we need to specify
% when the NN should shutdown. We do this by specifying the total number of cycles the NN should
% execute before terminating, which is 1000 in this case.
%
link_Cortex(Cx,IdsNPIds) ->

	% Determine 'cortex' GenoTypeId and PhenoTypeId.
	Cx_Id = Cx#cortex.id,
	Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),

	% Determine 'sensor' GenoTypeId list and PhenoTypeId list.
	SIds = Cx#cortex.sensor_ids,
	SPIds = [ets:lookup_element(IdsNPIds,SId,2) || SId <- SIds],

	% Determine 'neuron' GenoTypeId list and PhenoTypeId list.
	AIds = Cx#cortex.actuator_ids,
	APIds = [ets:lookup_element(IdsNPIds,AId,2) || AId <- AIds],

	% Determine 'neuron' GenoTypeId list and PhenoTypeId list.
	NIds = Cx#cortex.nids,
	NPIds = [ets:lookup_element(IdsNPIds,NId,2) || NId <- NIds],

	% Initialise the cortex (which in turn will initialise the other elements).
	Cx_PId ! {self(),{Cx_Id,SPIds,APIds,NPIds},1000}. % Allow 1000 pulses through the network.



% *************************************************************************************************
% For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id,
% and updates its weights.
%
% The convert_PIdPs2IdPs/3 performs the conversion from PIds to Ids of every {PId,Weights} tuple in
% the Input_PIdPs list.
%
% The updated Genotype is then returned back to the caller.
%

% --- Params ---
%
% IdsNPIds                  : The ETS table managing the genotype/phenotype id mapper.
% GenoType                  : The GenoType being updated.
% [{N_Id,PIdPs}|WeightPs]   : The {Id,Weights}
%
update_genotype(IdsNPIds,Genotype,[{N_Id,PIdPs}|WeightPs]) ->

	N = lists:keyfind(N_Id, 2, Genotype),
	io:format("PIdPs:~p~n",[PIdPs]),

	Updated_InputIdPs = convert_PIdPs2IdPs(IdsNPIds,PIdPs,[]),
	U_N = N#neuron{input_idps = Updated_InputIdPs},
	U_Genotype = lists:keyreplace(N_Id, 2, Genotype, U_N),
	io:format("N:~p~n U_N:~p~n Genotype:~p~n U_Genotype:~p~n",[N,U_N,Genotype,U_Genotype]),

	update_genotype(IdsNPIds,U_Genotype,WeightPs);
% Base Case - Return the Genotype
% --- return ---
%
% Genotype                  : The updated GenoType.
%
update_genotype(_IdsNPIds,Genotype,[]) ->
	Genotype.

convert_PIdPs2IdPs(IdsNPIds,[{PId,Weights}|Input_PIdPs],Acc) ->
	convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);

convert_PIdPs2IdPs(_IdsNPIds,[Bias],Acc) ->
	lists:reverse([{bias,Bias}|Acc]).