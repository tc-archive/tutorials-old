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




end