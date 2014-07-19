
# ***********************************************************************************************
# 
# - Write a function mapsum that takes a list and a function. It applies the function to each element 
#   of the list, and then sums the result, so
# 
#   iex> MyList.mapsum [1, 2, 3], &(&1 * &1) 14
# 
# 
# - Write max(list) that returns the element with the maximum value in the list. (This is slightly 
#   trickier than it sounds.)
# 
# 
# - An Elixir single quoted string is actually a list of individual character codes. Write a function 
#   caesar(list, n) that adds n to each element of the list, wrapping if the addition results in a 
#   character greater than z.
# 
#   iex> MyList.caesar('ryvkve', 13) ?????? :)
# 
#   Now report yourself to your national antiterror authority for creating encryption code. SHAME ON YOU.
# 
defmodule ListIt do

  @moduledoc """
  Some more recursive listy functions!
  """

  @doc """
  Write a function mapsum that takes a list and a function. It applies the function to each element 
  of the list, and then sums the result, so
 
  iex> MyList.mapsum [1, 2, 3], &(&1 * &1) 14
  """
  def mapsum(list, func), do: do_mapsum(list, 0, func)
  defp do_mapsum([], accum, _), do: accum
  defp do_mapsum([head|tail], accum, func), do: do_mapsum(tail, accum + func.(head), func)

  def mapsum_2([], _func), do: 0
  def mapsum_2([head|tail], func), do: func.(head) + mapsum_2(tail, func)


  # ***********************************************************************************************
  @doc """
  Write max(list) that returns the element with the maximum value in the list. (This is slightly 
  trickier than it sounds.)
 
  iex> MyList.mapsum [1, 2, 3], &(&1 * &1) 14
  """
  def max([head|tail]), do: do_max(tail, head)
  defp do_max([], max), do: max
  defp do_max([head|tail], max) when head > max, do: do_max(tail, head)
  defp do_max([head|tail], max) when head <= max, do: do_max(tail, max)


  # ***********************************************************************************************
  @doc """
  An Elixir single quoted string is actually a list of individual character codes. Write a function 
  caesar(list, n) that adds n to each element of the list, wrapping if the addition results in a 
  character greater than z.

  iex> MyList.caesar('ryvkve', 13) ?????? :)
  """

  # def caesar(charlist, n) when n >= 0, do: do_caesar(charlist, n)
  # defp do_caesar([], _n), do: [] 
  # defp do_caesar([head|tail], n) do
  #   [?A + rem(head - ?A + n, _range()) | do_caesar(tail, n)]
  # end


  def caesar(charlist, n, sc..ec \\ ?A..?z) 
    when ec > sc do 
    do_caesar(charlist, n, sc..ec)
  end

  def de_caesar(charlist, n, sc..ec \\ ?A..?z), do: do_caesar(charlist, (n * -1), sc..ec)

  defp do_caesar([], _n, _range), do: [] 
  defp do_caesar([head|tail], n, sc..ec) 
    # when n >= 0 do
      do
    [sc + rem(head - sc + n, ec - sc + 1) | do_caesar(tail, n, sc..ec)]
  end
  defp do_caesar([head|tail], n, sc..ec) 
    when n < 0 do
    [sc + rem(head + ec + n, ec - sc + 1) | do_caesar(tail, n, sc..ec)]
  end


  def caesar_acm(charlist, n), do: do_caesar_acm(charlist, n, [])
  defp do_caesar_acm([], _n, res), do: res 
  defp do_caesar_acm([head|tail], n, res) do
    do_caesar_acm(tail, n, res ++ [?A + rem(head - ?A + n, 58)])
  end

  def _range(), do: ?z - ?A + 1


  def charmap char1..char2 do
    for x <- char1..char2, do: {"#{[x]}", x}
  end



end

# for n <- ?A..?z, do:  IO.puts("#{n}, #{[n]}")


