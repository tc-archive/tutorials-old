defmodule Issues.GithubIssues do

  # @user_agent  [ {"User-agent", "Elixir dave@pragprog.com"} ]
  @user_agent  [ {"User-agent", "temple.cloud@gmail.com"} ]

  @doc"""
  Fetch the issues of the specified 'github' project.
  """
  def fetch(user, project) do
    issues_url(user, project)
    |> HTTPotion.get(@user_agent)
    |> handle_response
  end

  @doc"""
  Determine the gitub url for the specified project issues.
  """
  def issues_url(user, project) do
    "https://api.github.com/repos/#{user}/#{project}/issues"
  end

  @doc"""
  Handles a successful response.
  """
  def handle_response(%{status_code: 200, body: body}), do: { :ok, body }

  @doc"""
  Handles an error response.
  """
  def handle_response(%{status_code: ___, body: body}), do: { :error, body }

end