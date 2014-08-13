import :timer, only: [ sleep: 1 ]

defmodule MonitoredProcess do

  @doc"""
  Waits for 500ms then exits the process unceremoniously.
  """
  def sad_method do
    sleep 500
    exit(:boom)
  end


  @doc"""
  Runs 'sad_function' as a MONITORED child process.

  Because of this the :EXIT code of the terminating child process will be 
  trapped.
  """
  def run do

    res = spawn_monitor(MonitoredProcess, :sad_method, [])

    IO.puts inspect res

    receive do

      msg -> 
        IO.puts "MESSAGE RECEIVED: #{inspect msg}"

      after 1000 ->
        
        IO.puts "Nothing happened as far as I am concerned"

    end
  end
end

MonitoredProcess.run