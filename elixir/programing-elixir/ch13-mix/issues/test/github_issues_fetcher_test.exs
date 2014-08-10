defmodule GithubIssuesFetcherTest do

  use ExUnit.Case

  import Issues.GithubIssuesFetcher, only: [
    issues_url: 2
    ]


  # ***************************************************************************
  # Doc Tests!

  doctest Issues.GithubIssuesFetcher
  

  # ***************************************************************************
  # Unit Tests!
  
  test "Correct Github issues url generated." do
    assert issues_url("foo", "bar") == "https://api.github.com/repos/foo/bar/issues"
  end

end