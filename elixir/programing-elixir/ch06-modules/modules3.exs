# Write a function gcd(x,y) that finds the greatest common 
# divisor between two nonnegative integers. Algebraically, 
# gcd(x,y) is x if y is zero, gcd(y, rem(x,y)) otherwise.
defmodule GCDIt do 

  @moduledoc """
  Recursive calculation of the GCD of two integers.
  """

  @doc """
  Recursive calculation of the GCD of two integers.
  """
  def gcd(x, 0) when x >= 0, do: x
  def gcd(x, y) when x >= 0 and y >= 0, do: gcd(y, rem(x, y))

  @doc """
  Recursive calculation of the GCD of two integers.
  """
  def gcd_log(x, 0) when x >= 0, do: (
    IO.puts "gcd(#{x}, 0)"
    x
  )
  def gcd_log(x, y) when x >= 0 and y >= 0, do: (
    IO.puts "gcd(#{x}, #{y})"
    gcd_log(y, rem(x, y))
  )


end


# GCDIt.gcd(123455457234, 1934546456675871)
# GCDIt.gcd_log(123455457234, 1934546456675871)