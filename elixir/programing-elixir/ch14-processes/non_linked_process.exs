# ***************************************************************************
# 

import :timer, only: [ sleep: 1 ]       # Import the Erlang Timer


defmodule NonLinkedProcess do

  @doc"""
  Waits for 500ms then exits the process unceremoniously.
  """
  def sad_function do
    sleep 500
    exit(:boom)
  end


  @doc"""
  Runs 'sad_function' as a UNLINKED and UNTRAPPED child process.

  Because of this the :EXIT code of the terminating child process will not  
  be trapped and the 'timeout' message displayed.
  """
  def run do

    # Set the 'trap_exit' process flag to true.
    # NB: This is to ensure the child process is not linked in any way!
    Process.flag(:trap_exit, true)

    # Spawn an instance of the 'sad_function' child process.
    spawn(NonLinkedProcess, :sad_function, [])

    receive do

      # No :EXIT message when the spawned child process terminates (as the   
      # parent process is not 'linked' or 'monitored' and does not trap 
      # exits).
      msg -> 
        IO.puts "MESSAGE RECEIVED: #{inspect msg}"
      
      # ... or time-out after 1000ms. This should occur as we will not trap 
      # the ':boom' :EXIT signal.
      after 1000 ->
        IO.puts "Nothing happened as far as I am concerned"
   
    end

  end

end

NonLinkedProcess.run