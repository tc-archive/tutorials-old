defmodule GithubIssuesRendererTest do

  use ExUnit.Case

  import Issues.GithubIssuesRenderer, only: [
    extract: 1
    ]

  # ***************************************************************************
  # Doc Tests!

  doctest Issues.GithubIssuesRenderer
  

  # ***************************************************************************
  # Unit Tests!
  
  test "Correct data extracted from github issue HashDict." do
    # Mocks the input and compares with the expected result.
    {mock, expected} = github_issue_mock
    assert extract(mock) == expected
  end


  # Create a small partial mock to test the Issues.GithubIssuesRenderer,extract/1 
  # function.
  defp github_issue_mock do 

    # Some minimal fake data
    rec_num = "rec_num"; crtd_at = "crtd_at"; title = "title"

    # Json Tuple HashDict form... with additional key value
    mock = Enum.into(
      [{"number", rec_num}, {"foo", "bar"}, {"created_at", crtd_at}, {"title", title}], 
      HashDict.new
      )

    # Expected extracted Form
    expected = [rec_num, crtd_at, title]

    {mock, expected}

  end

end