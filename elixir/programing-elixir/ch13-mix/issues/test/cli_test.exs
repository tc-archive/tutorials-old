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
  # In this case just the specified functions.
  # 
  #
  import Issues.CLI, only: [ 
    parse_args: 1, 
    convert_to_list_of_hashdicts: 1,
    sort_into_ascending_order: 1
    ]

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


  @def"""
  Create a list of json-like datastructure to test the sort_into_ascending_order
  function.
  """
  defp created_at_list_fixture(values) do
    data = for value <- values, do: [{"created_at", value}, {"data", "somedata"} ]
    convert_to_list_of_hashdicts data
  end

  test "sort ascending orders the correct way" do
    fixture = created_at_list_fixture(["c", "a", "b"])
    result = sort_into_ascending_order(fixture)
    issues = for issue <- result, do: issue["created_at"]
    assert issues == ~w{a b c}
  end


end