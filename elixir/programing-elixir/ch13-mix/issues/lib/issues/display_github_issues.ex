defmodule Issues.DisplayGithubIssues do

  @moduledoc"""
  """

  @doc"""

  Input Data Format
  [[{"created_at", "c"}, {"data", "somedata"}],
  [{"created_at", "a"}, {"data", "somedata"}],
  [{"created_at", "b"}, {"data", "somedata"}]]


  """
  def display(list_of_hash_dicts) do 

    fmtr = Enum.reduce(list_of_hash_dicts, {0, 0, 0}, &build_fmtr(&1, &2))
    display_header(fmtr)

    Enum.each(list_of_hash_dicts, fn hash_dict -> display_hash_dict(hash_dict) end)

  end


#  #  | created_at           | title
# ----+----------------------+-----------------------------------------
# 889 | 2013-03-16T22:03:13Z | MIX_PATH environment variable (of sorts)
# 892 | 2013-03-20T19:22:07Z | Enhanced mix test --cover
# 893 | 2013-03-21T06:23:00Z | mix test time reports
# 898 | 2013-03-23T19:19:08Z | Add mix compile --warnings-as-errors

  # ***************************************************************************

  defp display_hash_dict(hash_dict) do 

    hash_dict 
      |> extract
      |> display_rec

  end


  def display_header({col1, col2, col3}) do 

    line = <<>>
    line = for x <- 0..col1, into: line, do: "-" 
    line = line <> "+" 
    line = for x <- 0..col2, into: line, do: "-" 
    line = line <> "+" 
    line = for x <- 0..col3, into: line, do: "-" 

    # IO.puts line

  end

  defp display_rec({rec_num, crtd_at, title}) do 
    IO.puts "#{rec_num}, #{crtd_at}, #{title}"
  end


  # ***************************************************************************
  
  def build_fmtr(list_of_hash_dicts) do 
    Enum.reduce(list_of_hash_dicts, {0, 0, 0}, &build_fmtr(&1, &2))
  end

  def build_fmtr(hash_dict, {col1_mx, col2_mx, col3_mx}) do 

    {rec_num, crtd_at, title} = extract(hash_dict)

    new_col1_mx = max(col1_mx, String.length(rec_num))
    new_col2_mx = max(col2_mx, String.length(crtd_at))
    new_col3_mx = max(col3_mx, String.length(title))

    {new_col1_mx, new_col2_mx, new_col3_mx}

  end


  # ***************************************************************************

  def extract(record_dict) do
    rec_num = "#{HashDict.get(record_dict, "number")}"
    crtd_at = "#{HashDict.get(record_dict, "created_at")}"
    title = "#{HashDict.get(record_dict, "title")}"
    {rec_num, crtd_at, title}
  end


end