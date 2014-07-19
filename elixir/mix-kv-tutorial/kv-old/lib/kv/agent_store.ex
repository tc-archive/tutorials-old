defmodule KV.AgentStore do


  	@doc """
  	Starts a new store with a HashDict
  	"""
  	# @spec start_link((() -> term), GenServer.options) :: on_start
	def start_link do
		Agent.start_link(fn -> HashDict.new end)
	end


  	@doc """
  	Puts the `value` for the given `key` in the `store`.
  	"""
	def put(agent, key, value, timeout \\ 5000) do
		Agent.update(agent, fn dict -> HashDict.put(dict, key, value) end, timeout)
		# Agent.update(agent, &HashDict.put(&1, key, value), timeout)
	end


  	@doc """
  	Get the `value` for the given `key` from the `store`.
  	"""
	def get(agent, key, timeout \\ 5000) do
		Agent.get(agent, fn dict -> HashDict.get(dict, key) end, timeout)
		# Agent.get(agent, &HashDict.get(&1, key), timeout)
	end


	@doc """
  	Delete the `value` for the given `key` from the `store`.
  	"""
	def delete(agent, key, timeout \\ 5000) do
		Agent.get_and_update(agent, fn dict -> HashDict.pop(dict, key) end, timeout)
		# Agent.get_and_update(agent, &HashDict.pop(&1, key), timeout)
	end



  	@doc """
  	Stop the agent.
  	"""
	def stop(agent) do
		Agent.stop(agent)
	end


end