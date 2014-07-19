defmodule KV.ProcessStore do

  def start do
    {:ok, spawn_link(fn -> loop(HashDict.new) end)}
  end


  @doc """
  Puts the `value` for the given `key` in the `store`.
  """
  def put(_caller, store, key, value) do
    IO.puts "PUT1"
    send store, {:put, key, value}
  end

  @doc """
  Get the `value` for the given `key` from the `store`.
  """
  def get(caller, store, key) do
    IO.puts "GET1"
    send store, {:get, key, caller}
  end

  @doc """
    Delete the `value` for the given `key` from the `store`.
    """
  def delete(caller, store, key) do
    send store, {:delete, key, caller}
  end


  defp loop(dict) do

    receive do

      {:put, key, value} ->
        IO.puts "PUT2"
        loop(HashDict.put(dict, key, value))

      {:get, key, caller} ->
        IO.puts "GET2"
        send caller, HashDict.get(dict, key, nil)
        loop(dict)

      {:delete, key, caller} ->
        value = HashDict.get(dict, key)
        new_dict = HashDict.delete(dict, key)
        send caller, value
        loop(new_dict)

      after 1_000 -> 
        IO.puts "AFTER"
        "nothing after 1s"

    end
  end

end