defmodule KV.ProcessStoreClient do

	def start do
		{:ok, spawn(fn -> loop end)}
	end


	@doc """
	Puts the `value` for the given `key` in the `store`.
	"""
	def put(client, store, key, value) do
	  	IO.puts "Client PUT:"
	  	KV.ProcessStore.put(client, store, key, value)
	end


	@doc """
	Get the `value` for the given `key` from the `store`.
	"""
	def get(client, store, key) do
		IO.puts "Client GET:"
		KV.ProcessStore.get(client, store, key)
	end


	@doc """
	Delete the `value` for the given `key` from the `store`.
	"""
	def delete(client, store, key) do
	    IO.puts "Client DELETE:"
	    KV.ProcessStore.delete(client, store, key)
	end



	defp loop() do

	    receive do

			nil -> 
				IO.puts "Recieved Nil" 
				nil

			val -> 
				IO.puts "Recieved"
				IO.puts "Val: #{val}" 
				val

			true ->
				IO.puts "Could not match." 

	      	after 1_000 -> 
	        	IO.puts "AFTER"
	        	"nothing after 1s"

	    end

  	end

end