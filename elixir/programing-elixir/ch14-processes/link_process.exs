# ***************************************************************************
# 
import :timer, only: [ sleep: 1 ]       # Import the Erlang Timer

defmodule LinkProcess do

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

    # Spawn up and instance of the 'sad_function' process - WITH A LINK!
    # This will cause an exit message to be sent back to this process when 
    # the child process terminates...
    spawn_link(LinkProcess, :sad_function, [])

    receive do

      # Receive a message... as a link has been specified we should 
      # receive this when the child process exits... :boom!
      msg -> 
        IO.puts "MESSAGE RECEIVED: #{inspect msg}"

      # ... Nah! We will time out first!
      after 1000 ->
        IO.puts "Nothing happened as far as I am concerned"

    end

  end

end

LinkProcess.run