%%%------------------------------------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erlang", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 11. Jul 2014 18:55
%%%------------------------------------------------------------------------------------------------
-module(simple_nn).
-author("Temple").

%% API
-export([]).
-compile(export_all).


% *************************************************************************************************
% Usage
%
% 1>c(simplest_nn). {ok,simplest_nn}
% 2>simplest_nn:create().
% true
% 3> cortex ! sense_think_act.
% ****Sensing****:
%  Signal from the environment [0.4435846174457203,0.7230402056221108]
% ****Thinking****
%  Input:[0.4435846174457203,0.7230402056221108]
%  with Weights:[-0.05641538255427969,0.2230402056221108,0.44581636451986995]
% sense_think_act
% ****Acting****:
%  Using:[0.5241599112258186] to act on environment.
%

% *************************************************************************************************
% The create function first generates 3 weights, with the 3rd weight being the Bias. The Neuron is
% spawned first, and is then sent the PIds of the Sensor and Actuator that it’s connected with.
% Then the Cortex element is registered and provided with the PIds of all the elements in the NN
% system.
%
%
% --- Erlang ---
%
% 1) Spawn a N_PId 'neuron' process for this module with the defined Weights and intially undefined
%    (input) 'sensor' and (output) 'actuator' processes. These will be specified in Step 3.
% 2) Spawn S_PId 'sensor' and A_PId 'actuator' process with a reference to the single N_PId neuron.
% 3) Send an 'init' message to the N_PId process providing the SPId and APId references.
% 4) Spawn and register a (CPId) 'cortex' process with SPId, NPId, and APId references.
%
% This effectively creates a complete 'single neuron network' with individual 'sensor', 'neuron'
% and 'actuator' processes governed by an individual 'cortex' process.
%
%                                  |---------- Cortex -----------|
%                                  |             |               |
%                                  V             V               V
%                                Sensor -----> Neuron -----> Actuator
%
create() ->
	Weights = [random:uniform()-0.5,random:uniform()-0.5,random:uniform()-0.5],
	N_PId = spawn(?MODULE,neuron,[Weights,undefined,undefined]),
	S_PId = spawn(?MODULE,sensor,[N_PId]),
	A_PId = spawn(?MODULE,actuator,[N_PId]),
	N_PId ! {init,S_PId,A_PId},
	register(cortex,spawn(?MODULE,cortex,[S_PId,N_PId,A_PId])).



% *************************************************************************************************
% The Cortex function triggers the sensor to action when commanded by the user. This process also
% has all the PIds of the elements in the NN system, so that it can terminate the whole system when
% requested.
%
%
% --- Erlang ---
%
% A 'reciever' method that is bound to the 'cortex' process.
%
% After initialisation it has reference  to the S_PId (sensor), N_PId (neuron), and, A_PId
% (actuator). It then has the cabability to manage and orchestrate the use of each process.
%
% The 'recieve block' matches on and perform the following messaging functions:
%
% 1) sense_think_act: Message the 'sensor' to initiate a 'pulse'.
% 2) terminate      : Message all manged processes to terminate.
%
cortex(Sensor_PId,Neuron_PId,Actuator_PId) ->     % Reference to three managed processes.
	receive

		sense_think_act ->
			Sensor_PId ! excite,                        % Message the 'sensor' to initiate a pulse event.
			cortex(Sensor_PId,Neuron_PId,Actuator_PId); % Keep alive!

		terminate ->                                  % Terminate all processes. Return ok.
			Sensor_PId ! terminate,
			Neuron_PId ! terminate,
			Actuator_PId ! terminate,
			ok

	end.


% *************************************************************************************************
% The 'sensor' process maintains a reference to a single neuron process. It has the capability
% to message a 'Sensory_Signal' to the the neuron process.
%
%
% --- Erlang ---
%
% A 'reciever' method that is bound to the 'sensor' process.
%
% After initialisation it has reference  to the N_PId (neuron) process.
%
% The 'recieve block' matches on and perform the following messaging functions:
%
% 1) excite         : Message the neuron process with a Sensory_Signal.
% 2) terminate      : Terminate the process. Return ok.
%
sensor(N_PId) ->
	receive

		excite ->
			Sensory_Signal = [random:uniform(),random:uniform()],   % Generate a random signal.
			log_signal(Sensory_Signal),
			N_PId ! {forward, self(), Sensory_Signal},              % Forward the signal to the neuron.
			sensor(N_PId);                                          % Keep alive!

		terminate ->                                              % Terminate this process.
			ok

	end.

log_signal(Sensory_Signal) ->
	io:format("****Sensing****:~n Signal from the environment ~p~n",[Sensory_Signal]).


% *************************************************************************************************
% After the neuron finishes setting its SPId and APId to that of the Sensor and Actuator
% respectively, it starts waiting for the incoming signals.
%
% The neuron expects a vector of length 2 as input, and as soon as the input arrives, the neuron
% processes the signal and passes the output vector to the outgoing APId.
%
%
% --- Erlang ---
%
% A 'reciever' method that is bound to the 'neuron' process.
%
% After initialisation it has reference  to the S_PId (sensor) process, A_PId (actuator) process,
% and a set of weights.
%
% The 'recieve block' matches on and perform the following messaging functions:
%
% 1) init         : Post initialise SPId and APId process reference
% 2) forward      : Process the specified input and forward the 'neural output' to the 'actuator'
%                   process.
% 3) terminate    : Terminate the process. Return ok.
%
neuron(Weights,S_PId,A_PId) ->
	receive

		{init, New_SPId, New_APId} ->                     % Set the updated S_PId and A_PId values.
			neuron(Weights,New_SPId,New_APId);              % Keep alive!

		{forward, S_PId, Input} ->                        % Process the input signal.
			log_input([Input,Weights]),
			Dot_Product = dot(Input,Weights,0),             % Determine the 'dot-product'.
			Output = [math:tanh(Dot_Product)],              % Determine the neural ouput.
			A_PId ! {forward, self(), Output},              % Forward the signal to the actuator.
			neuron(Weights,S_PId,A_PId);                    % Keep alive!

		terminate ->                                      % Terminate the process. Return ok.
			ok

	end.

log_input([Input,Weights]) ->
	io:format("****Thinking****~n Input:~p~n with Weights:~p~n", [Input,Weights]).


% *************************************************************************************************
% The dot function takes a dot product of two vectors, it can operate on a weight vector with and
% without a bias. When there is no bias in the weight list, both the Input vector and the Weight
% vector are of the same length. When Bias is present, then when the Input list empties out, the
% Weights list still has 1 value remaining, its Bias.
%
dot([I|Input],[W|Weights],Acc)
	-> dot(Input,Weights,I*W+Acc);
dot([],[],Acc)->
	Acc; dot([],[Bias],Acc)->
	Acc + Bias.



% *************************************************************************************************
% The Actuator function waits for a control signal coming from a Neuron. As soon as the signal
% arrives, the actuator executes its function, pts/1, which prints the value to the screen.
%
%
% --- Erlang ---
%
% A 'reciever' method that is bound to the 'actuator' process.
%
% After initialisation it has reference  to the N_PId (neuron) process.
%
% The 'recieve block' matches on and perform the following messaging functions:
%
% 1) forward      : Process the specified input and act on the environment,
% 2) terminate    : Terminate the process. Return ok.
%
actuator(N_PId) ->
	receive

		{forward,N_PId,Control_Signal} ->                   % Process the input signal.
			act(Control_Signal),                              % Act on the environment.
			actuator(N_PId);                                  % Keep alive!

		terminate ->                                        % Terminate the process. Return ok.
			ok

  end.

act(Control_Signal) ->
	io:format("****Acting****:~n Using:~p to act on environment.~n",[Control_Signal]).


