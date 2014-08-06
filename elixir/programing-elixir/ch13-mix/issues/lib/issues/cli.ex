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
  end


  @doc"""
  `argv` can be -h or --help, which returns :help.

  Otherwise it is a github user name, project name, and (optionally)
  the number of entries to format.

  Return a tuple of `{ user, project, count }`, or `:help` if help was given.
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
    case  parse  do

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