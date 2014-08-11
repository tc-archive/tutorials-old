
# Mapexamples from 'Programming Elixir' Book

defmodule MapIt do
  
  def values(dict) do
    dict |> Dict.values |> Enum.sum
  end 

end