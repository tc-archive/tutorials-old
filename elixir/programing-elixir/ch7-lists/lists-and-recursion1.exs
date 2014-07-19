# *************************************************************************************************
# I defined our sum function to carry a partial total around as a second parameter. I did this so I 
# could illustrate how to use accumulators to build values. The sum function can also be written 
# without an accumulator. Can you do it?

defmodule ListIt do 


  @moduledoc """
    Some listy functions!
  """

  @doc """
  Sums a list of integers
  """
  def sum([]), do: 0
  def sum([head|tail]) when is_number(head), do: head + sum(tail)


  def reduce([], accum, func), do: accum
  def reduce([head|tail], accum, func) do
    reduce(tail, func(head, acum), func)
  end

  def multiply(x, y), do: x * y

  def multiplier(list), do: reduce(list, 1, multiply)


end



defmodule MyList do

  @moduledoc """
    Some list functions from book.
  """

  def sum(list), do: _sum(list, 0)

  # private methods
  defp _sum([], total), do: total
  defp _sum([ head | tail ], total), do: _sum(tail, head+total) 

end