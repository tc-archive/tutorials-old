%%%-------------------------------------------------------------------
%%% @author Temple
%%% @author Gene I Sher
%%% @copyright (C) 2014, http://www.springer.com/computer/swe/book/978-1-4614-4462-6
%%% @doc
%%% Modified source code. Originally authored by Gene I Sher.
%%% "Handbook of Neuroevolution through Erland", ISBN 978-1-4614-4463-3,
%%% @end
%%% Created : 10. Jul 2014 19:51
%%%-------------------------------------------------------------------
-module(simple_neuron).
-author("Temple").

%% API
-export([]).
-compile(export_all).

% *************************************************************************************************
% Overview
%
% In the following algorithm, we spawn a process to represent our Neuron, and register it so that
% we can send and receive signals from it.
%
% We use a simple remote procedure call function called ‘sense’ to send signals to the registered
% neuron, and then receive the neuron’s output.



% *************************************************************************************************
% Usage
%
% 11> c(simple_neuron).
% {ok,simple_neuron}
% 12> simple_neuron:create().
% true
% 13> simple_neuron:sense([1,2]).
% * * * * Processing * * * *
%  Input:[1,2]
%  Using Weights:[-0.05641538255427969,0.2230402056221108,0.44581636451986995]
% Output:[0.6834082867332737]
% ok


% *************************************************************************************************
% The create function spawns a single neuron, where the weights and the bias are generated randomly
% to  be between -0.5 and 0.5.
%
% --- Erlang ---
%
% This initialiser function 'spawns' and 'registers' a new Erlang process called "neuron" which is
% initialised with the 'loop' function, and, the specified random input weights and bias.
%
% The specified 'loop function' contains a 'recieve block' that accepts tuples of the form
% {From, Input} where 'From' is the PId of the calling process and 'Input' an extended list of
% 'input signal' float values (that, in this static case with bias should be 3).
%
create() ->
	Weights = [random:uniform() - 0.5, random:uniform() - 0.5, random:uniform() - 0.5],
  register(neuron, spawn(?MODULE, loop, [Weights])).


% *************************************************************************************************
% The spawned neuron process accepts an input vector, prints it and the weight vector to the
% screen, calculates the output, and then sends the output to the contacting process.
%
% The output is also a vector of length one.
%
%
% --- Erlang ---
%
% The specified 'loop function' contains a 'recieve block' that accepts tuples of the form
% {From, Input} where 'From' is the PId of the calling process and 'Input' a list of 'input signal'
% float values (that, in this static case should be 3).
%
% Upon recieving a valid input tuple the output is calculated by caculating the 'dot product' of
% the incoming 'non-extended inputs' with the 'weights and bias'.
%
% This result is wrapped in a tuple and sent back to the calling PId, and the loop function
% recursively called to kepp it alive.
%
loop(Weights) ->                          % The input weights + bias.
  receive

    {From, Input} ->
      io:format("* * * * Processing * * * * ~n Input:~p~n Using Weights:~p~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],  % One element output
      From ! {result, Output},            % Return a tuple response
      loop(Weights)                       % Stay alive!

  end.


% *************************************************************************************************
% The dot product function that we use works on the assumption that the bias is incorporated into
% the weight list as the last value in that list. After calculating the dot product, the input list
% will empty out while the weight list will still have the single bias value remaining, which we
% then add to the accumulator.
%
%
% --- Erlang ---
%
% Recursively determine the dot-product of the input and weights with the base case handing the
% bias.
%
dot([I|Input], [W|Weights], Acc) ->       % List of Input, List of Weight + Bias, Accumulator
  dot(Input, Weights, I * W + Acc);       % Recursively determine the dot product.
dot([], [Bias], Acc)->                    % In the base case simply add the remaining 'bias'.
  Acc + Bias.


% *************************************************************************************************
% We use the sense function to contact the neuron and send it an input vector. The sense function
% ensures that the signal we are sending is a vector of length 2.
%
%
% --- Erlang ---
% If the specified Signal is a valid input to the neuron process; then forward the Signal to the
% neuron process with a callback reference to this process.
%
%
sense(Signal) ->                            % List of (2) input values

  case is_list(Signal) and (length(Signal) == 2) of
    true ->
      neuron ! {self(), Signal},            % If the input signal is valid send it to the registered
																						% 'neuron' process with 'self' as the callback process.
      receive {result, Output} ->
        io:format("Output:~p~n", [Output])  % Handle the result by displaying it.
      end;
    false ->
      io:format("The Signal must be a list of length 2~n")
  end.

