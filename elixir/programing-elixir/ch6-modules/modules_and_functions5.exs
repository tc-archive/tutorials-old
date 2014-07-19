# *************************************************************************************************
# Find the library functions to do the following, and then use each in iex. (If there’s the word 
# Elixir or Erlang at the end of the challenge, then you’ll find the answer in that set of 
# libraries.)
#
# – Convert a float to a string with 2 decimal digits. (Erlang)
#
# – Get the value of an operating system environment variable. (Elixir)
#
# – Return the extension component of a file name (so return .exs if given
#   "dave/test.exs" (Elixir)
#
# – Return the current working directory of the process. (Elixir)
#
# – Convert a string containing JSON into Elixir data structures. (Just
#   find, don’t install)
#
# – Execute a command in your operating system’s shell


# *************************************************************************************************
# Convert a float to a string with 2 decimal digits. (Erlang)
#
# :io.format("The number is ~3.2f~n", [15.6782])
f2s = fn flt, prsn -> :lists.nth(1, :io_lib.format("~.#{prsn}f", [flt]))  end
f2s.(2.345678, 2)  # '2.35'
f2s.(2.345678, 5)  # '2.34568'


# *************************************************************************************************
# Get the value of an operating system environment variable. (Elixir)
#
gsv = fn varname -> System.get_env(varname) end
gsv.("PATH")            # "/usr/local/Cellar/erlang/17.0/lib/erlang/erts-6.0/bin:/us... 
gsv.("USER")            # "Temple"


# *************************************************************************************************
# Return the extension component of a file name (so return .exs if given
# "dave/test.exs" (Elixir)
#
fext = fn path -> "." <> ((Regex.split ~r{[.]}, path) |> List.last) end
fext.("dave/test.exs")  # ".exs"


# *************************************************************************************************
# Return the current working directory of the process. (Elixir)
cdir = fn -> System.cwd() end
cdir.()                 # "/Users/Temple/Work/dev/ ...


# *************************************************************************************************
# Convert a string containing JSON into Elixir data structures. (Just
# find, don’t install)
#
# https://hex.pm/packages?search=json
# http://expm.co/?q=json


# *************************************************************************************************
# Execute a command in your operating system’s shell

rcmd = fn cmd -> System.cmd(cmd) end

rcmd.("ls -lA)")        # "Elixir.Times.beam\nElixir.Tim ...

rcmd_2 = fn cmd -> :os.cmd(cmd)
# This fails because the Erlang module function (which takes a 'String') expects 
# an Elixir 'charlist'
rcmd_2.("ls")           # ** (FunctionClauseError) no function clause matching in :os.validate/1
                        # (kernel) os.erl:361: :os.validate("ls")
                        # (kernel) os.erl:191: :os.cmd/1

# In elixir when you use double quotes you are specifying that it create a binary representation of a 
# string, and with single quotes you are requesting that it generate the list version. Most erlang 
# functions taking strings are going to expect the list version, and most Elixir functions are going 
# to expect the binary version. Some functions may accept either version.
rcmd_2.('ls')           # "Elixir.Times.beam\nElixir.Tim ..



# *************************************************************************************************
defmodule FuncIt do

  @moduledoc """
    Some funky functions!
  """
  

  @doc """
  Convert a float to a string with the specified precision.
  """
  def f2s(flt, prc \\ 2), do: :lists.nth(1, :io_lib.format("~.#{prc}f", [flt]))


  @doc """
  Convert a float to a string with the specified precision.
  """
  def f2s_2(flt, prc \\ 2) do
    [res|_] = :io_lib.format("~.#{prc}f", [flt])
    res
  end 

  def f2s_3(flt, prc \\ 2), do: :erlang.float_to_list(flt, [{:decimals, prc}])



  @doc """
  Get the value of an operating system environment variable. (Elixir)
  """
  def gsv(varname), do: System.get_env(varname)


  @doc """
  Return the extension component of a file name (so return .exs if given
  "dave/test.exs" (Elixir)
  """
  def fext(path), do: "." <> List.last(Regex.split ~r{[.]}, path)

  @doc """
  Return the extension component of a file name (so return .exs if given
  "dave/test.exs" (Elixir)
  """
  def fext_2(path), do: "." <> ((Regex.split ~r{[.]}, path) |> List.last)

  @doc """
  Return the extension component of a file name (so return .exs if given
  "dave/test.exs" (Elixir)
  """
  def fext_3(path), do: "." <> String.split(path, ".") |> tl |> hd

  @doc """
  Return the extension component of a file name (so return .exs if given
  "dave/test.exs" (Elixir)
  """
  def fext_4(path), do: Path.extname(path)


  @doc """
  Return the current working directory.
  """
  def cdir, do: System.cwd()



  @doc """
  Execute a command in your operating system’s shell. (Elixir)
  """
  def rcmd(cmd), do: System.cmd(cmd)

    @doc """
  Execute a command in your operating system’s shell. (Erlang)
  """
  def rcmd_2(cmd), do: :os.cmd(cmd)

  @doc """
  Execute a command in your operating system’s shell. (Elixir)
  """
  def rcmd_3(cmd), do: Mix.Shell.cmd(cmd)


end