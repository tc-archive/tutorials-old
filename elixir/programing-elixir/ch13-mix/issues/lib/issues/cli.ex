# Elixir Convention : 'mix module location' - Each module in it's own file in 'lib/${project_name}' dir.
# Elixir Conventon  : 'namespacing' - module name reflects directory structure.
defmodule Issues.CLI do

  # Module Attribute - Static
  @default_count 4

  @moduledoc"""
  Handle the command line parsing and the dispatch to
  the various functions that end up generating a 
  table of the last _n_ issues in a github project

  Example Usage:
  mix run -e 'Issues.CLI.run(["-h"])'
  mix run -e 'Issues.CLI.run(["elixir-lang", "elixir"])'
  mix run -e 'Issues.CLI.run(["wibble-wob", "wobble-wib"])'
  """
  # Default name of 'main' method for Elixir CLI modules.
  def run(argv) do
    argv
      |> parse_args
      |> process
  end

  @doc"""
  Process the 'help' switch.
  """
  def process(:help) do
    IO.puts """
      usage: issues <user> <project> [ count | #{@default_count} ] 
      """
    System.halt(0)
  end

  @doc"""
  Process the '{user, project, _count}' paramter tuple.
  """
  def process({user, project, _count}) do 
    Issues.GithubIssues.fetch(user, project)
    |> handle_response
  end

  @doc"""
  Handle a successful (json struct) response.
  """
  def handle_response({:ok, body}), do: body 

  @doc"""
  Handle a failed (json struct) response.
  """
  def handle_response({:error, body}) do
    {_, message} = List.keyfind(body, "message", 0) 
    IO.puts "Error fetching from Github: #{message}" 
    System.halt(2)
  end

  @doc"""
  The json that Github returns for a successful response is a list with one ele- ment per 
  GitHub issue. That element is itself a list of key/value tuples. To make these easier 
  (and more efficient) to work with, we’ll convert our list of lists into a list of 
  Elixir HashDicts.

  The HashDict library gives you fast access by key to a list of key/value pairs. 
  http://elixir- lang.org/docs/stable/HashDict.html
  """
  def convert_to_list_of_hashdicts(list) do
    list |> Enum.map(&Enum.into(&1, HashDict.new))
  end


  @doc"""
  `argv` can be -h or --help, which returns :help.

  Otherwise it is a github user name, project name, and (optionally)
  the number of entries to format.

  Return a tuple of `{user, project, count}`, or `:help` if help was given.
  """
  def parse_args(argv) do

    # Elixir built-in 'CLI Option Parser'
    #
    # - http://elixir-lang.org/docs/stable/OptionParser.html
    # - iex(1)> h OptionParser.parse
    #
    # Returns: The {[parsed values], [remaining arguments], [invalid options]}
    #
    parse = OptionParser.parse(argv, switches: [ help: :boolean],
                                     aliases:  [ h:    :help   ])
    case parse do

      {[ help: true ], _, _} -> 
        :help

      {_, [ user, project, count ], _}  -> 
        {user, project, String.to_integer(count)}

      {_, [ user, project ], _} -> 
        {user, project, @default_count}

      _ -> :help

    end
  end
end