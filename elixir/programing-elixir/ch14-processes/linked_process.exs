# ***************************************************************************
# 
# elixir -r linked_process.exs  -e LinkedProcess.run
#
import :timer, only: [ sleep: 1 ]       # Import the Erlang Timer

defmodule LinkedProcess do

  @doc"""
  Waits for 500ms then exits the process unceremoniously.
  """
  def sad_function do
    sleep 500
    exit(:boom)
  end

  @doc"""
  Runs 'sad_function' as a LINKED child process.

  Because of this the :EXIT code of the terminating child process will be 
  trapped.
  """
  def run do

    # Spawn and LINK an instance of the 'sad_function' child process.
    # This will cause an exit message to be sent back to this process when 
    # the child process terminates...
    spawn_link(LinkedProcess, :sad_function, [])

    receive do

      # An :EXIT message is received when the spawned child process  
      # terminates (as the child process is 'linked' to this parent 
      # process).
      msg -> 
        IO.puts "MESSAGE RECEIVED: #{inspect msg}"

      # ... Nah! We will ':boom' first.
      after 1000 ->
        IO.puts "Nothing happened as far as I am concerned"

    end

  end

end

LinkedProcess.run