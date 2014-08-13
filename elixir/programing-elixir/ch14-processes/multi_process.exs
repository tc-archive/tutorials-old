# ***************************************************************************
# Write a program that spawns two processes, and then passes each a unique 
# token (for example “fred” and “betty”). Have them send the tokens back.
#
#   – Is the order that the replies are received deterministic in theory? 
#     In practice?
#   – If either answer is no, how could you make it so?
#
#     No. Theoretically not deterministic  - as we do no know how long it   
#     will take to process each message. Although, in practice if the 
#     task carried out in the first process is faster to complete than 
#     second, then it it will return first.
#
#     A way to oder the responses might be to identify each process 
#
#
# NB: iex> :pman.start
#
defmodule MultiProcess do

  @doc"""
  A simple 'process' method identifiable by an id.

  Accepts a simple {pid, msg} tuple that simply returns the 'msg'.
  Accepts a ':shutdown' method to escape process recursion.
  """
  def process(id) do

    receive do 

      {sender_pid, msg} when is_pid(sender_pid) ->

        wait = rnd(1000)
        IO.puts "Process '#{id}' waiting for (#{wait})."
        :timer.sleep(wait)

        IO.puts "(#{wait}) Replying from Process '#{id}'..."
        send sender_pid, {id, msg}
        process(id)

      :shutdown ->
        IO.puts "Shutting down - Process '#{id}'."

    end

  end

  @doc"""
  Return a random value between 0..Limit.
  """
  def rnd(limit) do
    :random.seed(:erlang.now())
    wait = :random.uniform(limit)
  end

end


# ***************************************************************************
# Spawn two processes with special identifiers
#
fred_pid = spawn(MultiProcess, :process ,[:fred])
betty_pid = spawn(MultiProcess, :process ,[:betty])


# ***************************************************************************
# Send a message to 'fred_pid' first, then 'betty_pid'.
#
# Display the returned results in any order...
#
send fred_pid, {self, "Hello Fred!"}
send betty_pid, {self, "Hello Betty!"}

receive do
  {_, data} -> IO.puts data
end

receive do
  {_, data} -> IO.puts data
end


# ***************************************************************************
# Send a message to 'fred_pid' first, then 'betty_pid'.
#
# Use receive blocks to display the result from 'betty_pid' first; then from 
# 'fred_pid' (even though we sent to 'fred_pid' first).
#
send fred_pid, {self, "Hello again Fred!"}
send betty_pid, {self, "Hello again Betty!"}

receive do
  {:betty, data} -> IO.puts data
end

receive do
  {:fred, data} -> IO.puts data
end


# ***************************************************************************
# Shutdown the processes...
#
send fred_pid, :shutdown
send betty_pid, :shutdown

Process.alive?(fred_pid)
Process.alive?(betty_pid)