defmodule Chain do

  # ***************************************************************************

  # Max Default Erlang Processes: 262143
  # elixir --erl "+P 10000000" -r chain.exs -e "Chain.run(10000000)"

  def run(n) do
    IO.puts inspect :timer.tc(Chain, :create_processes, [n])
  end


  def create_processes(n) do
    last = Enum.reduce 1..n, self, 
             fn (_,send_to) -> 
               spawn(Chain, :counter, [send_to]) 
             end 

    # start the count by sending
    send last, 0

    # and wait for the result to come back to us
    receive do
      final_answer when is_integer(final_answer) -> 
        "Result is #{inspect(final_answer)}"
    end

  end


  def counter(next_pid) do    
    receive do
      n -> 
        send next_pid, n + 1
    end
  end
    
end