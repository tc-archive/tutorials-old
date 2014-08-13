# ***************************************************************************
# 
# elixir -r linked_process.exs  -e LinkedProcess.run
#
import :timer, only: [ sleep: 1 ]       # Import the Erlang Timer

defmodule LinkedProcess do

  @moduledoc"""

  LINKING joins the calling process and another process—each receives 
  notifications about the other. 

  NB: By contrast, MONITORING lets a process spawn another and be notified of 
      its termination, but without the reverse notification —it is one way only.

  
  SAFETY WARNING

  If you monitor (or link to) an existing process, there is a potential race 
  condition if the other process dies before your monitor call completes, you 
  may not receive a noti- fication. The spawn_link and spawn_monitor versions 
  are atomic, however, so you’ll always catch a failure.
  """


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
    # This will cause an :EXIT message to be sent back to this process when 
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