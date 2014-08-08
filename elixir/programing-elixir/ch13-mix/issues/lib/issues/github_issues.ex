defmodule Issues.GithubIssues do


  # A module attribute to fetch the 'github_url' value at compile time.
  #
  # Please see:  $/config.config.exs
  @github_url Application.get_env(:issues, :github_url)

  @user_agent [{"User-agent", "temple.cloud@gmail.com"}]


  @doc"""
  Fetch the issues of the specified 'github' project.
  """
  def fetch(user, project) do
    issues_url(user, project)
    |> HTTPotion.get(@user_agent)
    |> handle_response
    |> json_decode
    |> convert_to_list_of_hashdicts
    |> sort_into_ascending_order
  end

  @doc"""
  Determine the gitub url for the specified project issues.
  """
  def issues_url(user, project) do
    "#{@github_url}/repos/#{user}/#{project}/issues"
  end

  @doc"""
  Handles a successful response.
  """
  def handle_response(%{status_code: 200, body: body}), do: {:ok, body}

  @doc"""
  Handles an error response.
  """
  def handle_response(%{status_code: ___, body: body}), do: {:error, body}

  @doc"""
  Convert the 'body' response json string; to an elixir data structure.
  """
  def json_decode({status_code, body_str}) do
    {status_code, :jsx.decode(body_str)}
  end


  @doc"""
  The json that Github returns for a successful response is a list with one element 
  per GitHub issue. That element is itself a list of key/value tuples. To make these 
  easier (and more efficient) to work with, we’ll convert our list of lists into a 
  list of Elixir HashDicts.
  """
  def convert_to_list_of_hashdicts(list) do
    list |> Enum.map(&Enum.into(&1, HashDict.new))
  end

  @doc"""
  """
  def sort_into_ascending_order(list_of_issues) do 
    Enum.sort list_of_issues, fn i1, i2 -> i1["created_at"] <= i2["created_at"] end
  end
  

end