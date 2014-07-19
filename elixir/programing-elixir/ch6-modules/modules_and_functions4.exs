# I’m thinking of a number between 1 and 1000...
# The most efficient way to find the number is to guess 
# halfway between the low and high numbers of the range. If 
# our guess is too big, then the answer lies between the 
# bottom of the range and one less than our guess. If it is 
# too small, then the answer lies between one more than our 
# guess and the end of the range.

# Code this up. Your API will be guess(actual, range), where 
# range is an Elixir range.

# Your output should look similar to:
#     iex> GuessIt.guess(273, 1..1000)
#     Is it 500
#     Is it 250
#     Is it 375
#     Is it 312
#     Is it 281
#     Is it 265
#     Is it 273
#     273

# Hints:
# – You may need to implement helper functions with an 
#   additional parameter (the currently guessed number).
# – the div(a,b) function performs integer division
# – guard clauses are your friends
# – patterns can match the low and high parts of a range 
#   (a..b=4..8)

defmodule GuessIt do 

  @author temple

  @moduledoc """
  A module containing binary search type play functions.
  """

  @doc """
  Attempts to return the target via a binary search.
  """
  def guess(target, fst..lst) when fst < lst and target >= fst and target <= lst do
    do_guess(middle(fst..lst), fst..lst, target)
  end


  defp do_guess(guess, fst..lst, target) when guess == target do
     IO.puts "It is #{guess}!"
     guess
  end
  defp do_guess(guess, fst..lst, target) when guess > target do
     IO.puts "Is it #{guess}? No that is too high..."
     newrange = fst..guess-1
     do_guess(middle(newrange), newrange, target)
  end
  defp do_guess(guess, fst..lst, target) when guess < target do
     IO.puts "Is it #{guess}? No that is too low..."
     newrange = guess+1..lst
     do_guess(middle(newrange), newrange, target)
  end


  defp middle(fst..lst) do
     middle = div(fst + lst, 2)
     # IO.puts "Middle is #{middle} (#{fst}..#{lst})"
     middle
  end




end