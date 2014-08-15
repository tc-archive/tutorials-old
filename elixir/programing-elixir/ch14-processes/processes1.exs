# ***************************************************************************
# The Erlang function timer.sleep(time_in_ms) suspends the current process 
# for a given time. You might want to use it to force some scenarios in the 
# following exercises. The key with the exercises is to get used to the 
# different reports that you’ll see when you’re developing code.
#
# - Use spawn_link to start a process, and have that process send a message to 
#   the parent and then exit immediately. Meanwhile, sleep for 500ms in the 
#   parent, then receive as many messages as there are waiting. Trace what you 
#   receive. Does it matter that you weren’t waiting for the notification from 
#   the child at the time it exited?
#
#   * Does not matter the child is sleeping... the exit is stil received
#
#
# - Do the same, but have the child raise an exception. What difference do you 
#   see in the tracing.
#
# - Repeat the two, changing spawn_link to spawn_monitor.
#
#

defmodule ProcessIt do


    def process1(parent_pid) do
      IO.puts "Child 'process1' sending message..."
      send parent_pid, :dude
    end

    # Does not matter the child is sleepin. The exit is still received.
    def process2(parent_pid) do
      IO.puts "Child 'process2' sending message..."
      send parent_pid, :dude
      IO.puts "Child 'process2' exiting..."
      exit(:gameover)
    end


    def sleep(time_in_ms) do
      :timer.sleep(time_in_ms)
    end

end


spawn_link(ProcessIt, :process1, [self])

IO.puts "Parent sleeping..."
ProcessIt.sleep(5000)
IO.puts "Parent waking..."

IO.puts "Parent receiving..."
receive do
  :dude -> 
    IO.puts "I know what time it is..."
end
IO.puts "Parent received msg."



spawn_link(ProcessIt, :process2, [self])

IO.puts "Parent sleeping..."
ProcessIt.sleep(5000)
IO.puts "Parent waking..."

IO.puts "Parent receiving..."
receive do
  :dude -> 
    IO.puts "I know what time it is..."
end
IO.puts "Parent received"

