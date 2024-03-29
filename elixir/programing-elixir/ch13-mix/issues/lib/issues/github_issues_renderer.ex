defmodule Issues.GithubIssuesRenderer do

  @moduledoc"""
  A module to render the output of github issues to the console in tabular 
  form.
  """

  import Issues.TabularDataGenerator, only: [
    tbl_row_border: 1,
    tbl_row_data: 2,
    ]


  # The default 'table header' for the 'github issue' data.
  @table_header ["#", "created_at", "title"]


  # ***************************************************************************

  @doc"""
  Takes a list of HashDicts where each Hashdict is an 'issue' from a 
  specified github repository.

  It then displays each issue in a tabular from to the ouput console.

   #  | created_at           | title
  ----+----------------------+-----------------------------------------
  889 | 2013-03-16T22:03:13Z | MIX_PATH environment variable (of sorts)
  892 | 2013-03-20T19:22:07Z | Enhanced mix test --cover
  893 | 2013-03-21T06:23:00Z | mix test time reports
  898 | 2013-03-23T19:19:08Z | Add mix compile --warnings-as-errors

  """
  def display(issues) do 

    fmtr = create_fmtr(issues)

    IO.puts tbl_row_data(@table_header, fmtr)
    IO.puts tbl_row_border(fmtr)
    Enum.each(issues, 
      fn (issue) -> IO.puts tbl_row_data(extract(issue), fmtr) end
      )

  end

  # ***************************************************************************

  @doc"""
  Extract the 'number', 'created_at' and 'title' fields of the specified 
  HashDict input and converts it to a list of strings.
  """
  def extract(record_dict) do
    rec_num = "#{HashDict.get(record_dict, "number")}"
    crtd_at = "#{HashDict.get(record_dict, "created_at")}"
    title = "#{HashDict.get(record_dict, "title")}"
    [rec_num, crtd_at, title]
  end


  # ***************************************************************************

  @doc"""
  Creates a special 'cloumn formater' with respect to the specified input data.

  It scan trhough the data and determines the 'maximum width' of each column 
  w.r.t to the largest sized data item in each column of the input set.
  """
  def create_fmtr(issues) do 
    {col1_mx, col2_mx, col3_mx} = get_max_cols_widths(issues)
    [col1_mx + 2, col2_mx + 2, col3_mx + 2]
  end

  # Finds the maximum column width of each column w.r.t the input data.
  defp get_max_cols_widths(issues) do 
    Enum.reduce(issues, {0, 0, 0}, &get_max_cols_widths(&1, &2))
  end

  # Finds the maximum column width of each column w.r.t the input data.
  defp get_max_cols_widths(issue, {col1_mx, col2_mx, col3_mx}) do 

    [rec_num, crtd_at, title] = extract(issue)

    new_col1_mx = max(col1_mx, String.length(rec_num))
    new_col2_mx = max(col2_mx, String.length(crtd_at))
    new_col3_mx = max(col3_mx, String.length(title))

    {new_col1_mx, new_col2_mx, new_col3_mx}

  end


end