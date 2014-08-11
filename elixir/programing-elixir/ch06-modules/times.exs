# Elixir Module Definition

defmodule Times do 
	def double(n) do
		n*2
	end 
end

# The do...end form is just a lump of syntactic sugar—during 
# compilation it is turned into the do: form.2 Typically 
# people use the do: syntax for single line blocks, 
# and do...end for multiline ones.
# 
# defmodule Times do
#   def double(n), do: n * 2
# end
#
#
# defmodule Times, do: (def double(n), do: n*2)