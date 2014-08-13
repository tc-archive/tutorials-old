# ***************************************************************************
# 

import :timer, only: [ sleep: 1 ]       # Import the Erlang Timer


defmodule Link1 do

  @doc"""
  Waits for 500ms then exits the process unceremoniously.
  """
  def sad_function do
    sleep 500
    exit(:boom)
  end


  @doc"""
  Runs 'sad_function', but, with 'trap_exit' process flag set to true.

  Because of this no message will be recieved.
  """
  def run do

    # Set the 'trap_exit' process flag to true.
    Process.flag(:trap_exit, true)

    # Spawn up and instance of the 'sad_function' process.
    spawn(Link1, :sad_function, [])

    receive do

      # Receive a message...
      msg -> 
        IO.puts "MESSAGE RECEIVED: #{inspect msg}"
      
      # ... or time-out after 1000ms
      after 1000 ->
        IO.puts "Nothing happened as far as I am concerned"
   
    end

  end

end