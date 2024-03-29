# *************************************************************************************************
# - Implement the following Enum functions using no library functions or list comprehensions: 
# all?, each, filter, split, and take. 
# 
# You may need to use an if statement to implement filter. The syntax for this is:
#
# if condition do 
#   expression(s)
# else
#   expression(s)
# end
#
# 
# - (Harder) Write a function flatten(list) that takes a list that may contain any number of 
# sublists, and those sublists may contain sublists, to any depth. It returns the elements of these 
# lists as a flat list.
# 
# iex> MyList.flatten([ 1, [ 2, 3, [4] ], 5, [[[6]]]]) [1,2,3,4,5,6]
#
# Hint: You may have to use Enum.reverse to get your result in the correct order.

defmodule Cols do 

  # ***********************************************************************************************
  @doc"""
  Invokes the given fun for each item in the collection and returns false if at least one invocation 
  returns false. Otherwise returns true.
  """
  # iex(13)> Cols.all?([1,2,3,4], &(&1 < 5))
  # true
  # iex(14)> Cols.all?([1,2,3,4,5], &(&1 < 5))
  # false
  def all?([], _fun),  do: true 
  def all?([h|t], fun \\ fn x -> x end), do: fun.(h) && all?(t, fun)


  @doc"""
  Invokes the given fun for each item in the collection. Returns :ok
  """
  # iex(36)> Cols.each([1,2,3,4,5], &(IO.puts &1))
  # 1 2 3 4 5
  # :ok
  def each([], _fun), do: :ok 
  def each([h|t], fun \\ fn x -> x end) do 
    fun.(h)
    each(t, fun)
  end


  @doc"""
  Filters the collection, i.e. returns only those elements for which fun returns true.
  """
  # iex(55)> Cols.filter([1,2,3,4,5], &(rem(&1,2) == 0))
  # [2, 4]
  def filter(collection, fun), do: _filter(collection, [], fun) 
  defp _filter([], acc, _fun), do: acc 
  defp _filter([h|t], acc, fun) do 
    if fun.(h) do
      _filter(t, acc ++ [h], fun)
    else
      _filter(t, acc, fun)
    end
  end


  @doc"""
  Splits the enumerable into two collections, leaving count elements in the first one. If count 
  is a negative number, it starts counting from the back to the beginning of the collection.
  """
  # def split(collection, count) when count < 0 do
  #   [acc1, acc2] = split(Enum.reverse(collection), count * -1)
  #   [Enum.reverse(acc2), Enum.reverse(acc1)]
  # end
  def split(collection, count) when count < 0 do 
    _split(collection, count + length(collection), [[],[]])
  end
  def split(collection, count) when count >= 0, do: _split(collection, count, [[],[]])
  defp _split([h|t], count, [acc1, acc2]) do
    if count > 0 do
      _split(t, count-1, [acc1 ++ [h], acc2])
    else
      _split(t, count, [acc1, acc2 ++ [h]])
    end
  end
  defp _split([], _count, [acc1, acc2]), do: [acc1, acc2]


  def split_2(list, count),      do: _split_2(list, [], count)
  defp _split_2([], front, _),   do: [ Enum.reverse(front), [] ]
  defp _split_2(tail, front, 0), do: [ Enum.reverse(front), tail ]
  defp _split_2([ head | tail ], front, count)  do
    _split_2(tail, [head|front], count-1)
  end



  @doc"""
  Takes the first count items from the collection.
  """
  # iex(68)> Cols.take([1,2,3,4,5], 3)
  # [1, 2, 3]
  # iex(69)> Cols.take([1,2,3,4,5], 7)
  # [1, 2, 3, 4, 5]
  # iex(70)> Cols.take([1,2,3,4,5], -1)
  # []
  # iex(71)> Cols.take([], 3)
  # []
  def take(collection, count), do: _take(collection, count, []) 
  defp _take([h|t], count, acc) when count > 0, do: _take(t, count-1, acc ++ [h])
  defp _take(_, count, acc) when count <= 0, do: acc 
  defp _take([], _count, acc), do: acc 

  def take_2(collection, count), do: split(collection, count) |> hd



  @doc"""
  Takes a list that may contain any number of sublists, and those sublists may contain sublists, 
  to any depth. It returns the elements of these lists as a flat list.
  """
  # iex(52)> Cols.flatten([[1, "a", ["i", "ii", "iii"]], 2, 3])
  # [1, "a", "i", "ii", "iii", 2, 3]
  def flatten(collection), do: _flatten(collection, [])
  defp _flatten([head|tail], acc) when is_list(head), do: _flatten(tail, _flatten(head, acc))
  defp _flatten([head|tail], acc), do: _flatten(tail, acc ++ [head])
  defp _flatten([], acc), do: acc


end