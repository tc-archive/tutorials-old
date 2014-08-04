defmodule ListIt do 

  @moduledoc """
    Some listy functions!
  """

  # ***********************************************************************************************
  # I defined our sum function to carry a partial total around as a second parameter. I did this so
  # I could illustrate how to use accumulators to build values. The sum function can also be written 
  # without an accumulator. Can you do it?

  @doc """
  Sums a list of integers
  """
  def sum([]), do: 0
  def sum([head|tail]) when is_number(head), do: head + sum(tail)


  # ***********************************************************************************************
  # Messing with reducer fuctions and syntax choices...

  @doc """
  Reducer with accumulator higher order function.
  """
  def reduce([], accum, _), do: accum
  def reduce([head|tail], accum, func), do: reduce(tail, func.(head, accum), func)

  @doc """
  Reducer with private modules function.
  """
  def multiplier(list), do: reduce(list, 1, &multiply/2)

  defp multiply(x, y), do: x * y

  @doc """
  Reducer with anonymous function.
  NB: Command line example: 
      iex(8)> adder = fn (list) -> ListIt.reduce(list, 0, &(&1 + &2)) end
  """
  def adder(list), do: reduce(list, 0, &(&1 + &2))

end