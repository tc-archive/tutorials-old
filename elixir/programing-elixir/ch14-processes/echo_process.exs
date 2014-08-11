# Spawn a process and invoke a zero-argument function.

defmodule EchoProcess do

  # An 'echo' function that returns the specified 'msg' to the client 
  # process. (NB: This is sort of the name of the 'channel')
  #
  # State can be stored in the function closure.
  #
  # NB: This makes this a bit like an 'object' in 'oo-languages'?
  #
  def echo(process_name \\ "Default Echo Process") do

    # The 'receive block' declares a message channel to pattern match 
    # incoming messages against. 
    receive do
      # Match against a 2-ary tuple:
      #
      #   send: PId of the sending process (client).
      #   msg : The message sent.
      #
      {sender, msg} ->

        # Send (echo!) back to the client the original message.
        # (with an :ok status atom).
        send sender, { :ok, "#{process_name}: #{msg}" }

        # A process will exit after a single message been received.
        # So, to keep it running (if required) we must re-invoke the 
        # the function/channel.
        echo(process_name)

      # Match against an :atom. In this case eist the process.
      #
      :shutdown ->
        IO.puts "Shutting down..."

      # A timeout period after which a single clause is executed.
      #
      after 10000 ->
        IO.puts "Nothing to do. Shutting down..."

        # The process exits on the completion of the receive block.

    end

  end

end

# Spawn the EchoProcess
# NB: spawn(ModuleName, :function_atom, InitArgList)
pid = spawn(EchoProcess, :echo, ["Echo Process"])

# Send a message to the echo process.
send pid, {self, "I am alive!"}

# Await a response!
receive do
  {:ok, message} ->
    IO.puts message
end


# Send a message to the echo process.
send pid, {self, "I am still alive!"}

# Await a response!
receive do
  {:ok, message} ->
    IO.puts message
end

# Send a message to the echo process.
send pid, :shutdown