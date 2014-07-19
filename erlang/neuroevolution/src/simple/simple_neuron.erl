%%%-------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2014 19:51
%%%-------------------------------------------------------------------
-module(simple_neuron).
-author("Temple").

%% API
-export([]).
-compile(export_all).


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
create() ->
	Weights = [random:uniform() - 0.5, random:uniform() - 0.5, random:uniform() - 0.5],
  register(neuron, spawn(?MODULE, loop, [Weights])).


% *************************************************************************************************
% The spawned neuron process accepts an input vector, prints it and the weight
% vector to the screen, calculates the output, and then sends the output to the
% contacting process. The output is also a vector of length one.
%
loop(Weights) ->
  receive

    {From, Input} ->
      io:format("* * * * Processing * * * * ~n Input:~p~n Using Weights:~p~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      From ! {result, Output}, 
      loop(Weights)

  end.


% *************************************************************************************************
% The dot product function that we use works on the assumption that the bias is incorporated into
% the weight list as the last value in that list. After calculating the dot product, the input list
% will empty out while the weight list will still have the single bias value remaining, which we
% then add to the accumulator.
%
dot([I|Input], [W|Weights], Acc) -> 
  dot(Input, Weights, I * W + Acc);
dot([], [Bias], Acc)-> 
  Acc + Bias.


% *************************************************************************************************
% We use the sense function to contact the neuron and send it an input vector. The sense function
% ensures that the signal we are sending is a vector of length 2.
%
sense(Signal) ->

  case is_list(Signal) and (length(Signal) == 2) of
    true ->
      neuron ! {self(), Signal},
      receive {result, Output} ->
        io:format("Output:~p~n", [Output])
      end;
    false ->
      io:format("The Signal must be a list of length 2~n")
  end.

