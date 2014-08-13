# ***************************************************************************
# 
# elixir -r monitored_process.exs -e MonitoredProcess.run
#
import :timer, only: [ sleep: 1 ]

defmodule MonitoredProcess do


  @moduledoc"""

  MONITORING lets a process spawn another and be notified of its 
  termination, but without the reverse notification —it is one way only.

  NB: By contrast, LINKING joins the calling process and another process —   
      each receives notifications about the other.  


  SAFETY WARNING

  If you monitor (or link to) an existing process, there is a potential race 
  condition if the other process dies before your monitor call completes, you 
  may not receive a noti- fication. The spawn_link and spawn_monitor versions 
  are atomic, however, so you’ll always catch a failure.
  """



  @doc"""
  Waits for 500ms then exits the process unceremoniously.
  """
  def sad_method do
    sleep 500
    exit(:boom)
  end


  @doc"""
  Runs 'sad_function' as a MONITORED child process.

  Because of this the :DOWN code of the terminating child process will be 
  trapped.
  """
  def run do

    # Spawn and LINK an instance of the 'sad_function' child process.
    # This will cause an :DOWN message to be sent back to this process when 
    # the child process terminates...
    res = spawn_monitor(MonitoredProcess, :sad_method, [])

    IO.puts inspect res

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

MonitoredProcess.run