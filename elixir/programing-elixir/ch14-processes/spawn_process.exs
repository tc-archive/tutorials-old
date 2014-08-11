# Spawn a process and invoke a zero-argument function.

defmodule SpawnProcess do
  def greet do
    IO.puts "Hello"
  end
end

# Start IEx and compile the module file.
c("spawn_process.ex") # [SpawnProcess]

# First, let’s call it as a regular function:
SpawnProcess.greet Hello # :ok

# Now, let’s run it in a separate process:
# NB: spawn(ModuleName, :function_atom, ArgList)
spawn(SpawnProcess, :greet, []) # Hello # PID<0.42.0>
