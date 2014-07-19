# Extend the Times module with a triple function, that 
# multiplies its parameter by three.

defmodule TimesIt do

  @moduledoc """
  A simple multiplication module.
  """


  @doc """
  Double the specified numerical input.
  """
  def double(n), do: n * 2

  @doc """
  Triple the specified numerical input.
  """
  def triple(n), do: n * 3

  @doc """
  Quadrupal the specified numerical input.
  """
  def quadrupal(n), do: double(double(n))

end