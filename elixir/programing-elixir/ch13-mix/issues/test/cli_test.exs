defmodule CliTest do

  # Use the 'ExUnit.Case' helper module.
  #
  # - iex(1)> h use
  #   use is a simple mechanism for using a given module into the current context.
  #
  # Make 'assert' macro available for tests.
  #
  use ExUnit.Case


  # Import the 'Issues.CLI' class to test.
  #
  # - iex(1)> h import
  #   Imports function and macros from other modules.
  #
  # In this case just the 'Issues.CLI.parse_args/1' function.
  # 
  #
  import Issues.CLI, only: [ parse_args: 1 ]


  # Tests!
  
  test ":help returned by option parsing with -h and --help options" do
    assert parse_args(["-h",     "anything"]) == :help
    assert parse_args(["--help", "anything"]) == :help
  end

  test "three values returned if three given" do
    assert parse_args(["user", "project", "99"]) == { "user", "project", 99 }
  end

  test "count is defaulted if two values given" do
    assert parse_args(["user", "project"]) == { "user", "project", 4 }
  end
end