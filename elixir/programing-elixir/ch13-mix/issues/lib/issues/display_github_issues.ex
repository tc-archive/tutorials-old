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

    # fmtr = build_fmtr(list_of_hash_dicts)
    # IO.puts display_row({"#", "created_at", "title"}, fmtr)
    # IO.puts display_header(fmtr)
    # Enum.each(list_of_hash_dicts, 
    #   fn (hash_dict) -> IO.puts display_row(extract(hash_dict), fmtr) end
    #   )

    fmtr = build_fmtr(list_of_hash_dicts)
    IO.puts tbl_row_data(["#", "created_at", "title"], fmtr)
    IO.puts tbl_row_border(fmtr)
    Enum.each(list_of_hash_dicts, 
      fn (hash_dict) -> IO.puts tbl_row_data(extract(hash_dict), fmtr) end
      )


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


  def display_header([col1, col2, col3]) do 

    line = <<>>
    line = for _ <- 0..col1, into: line, do: "-" 
    line = line <> "+" 
    line = for _ <- 0..col2, into: line, do: "-" 
    line = line <> "+" 
    line = for _ <- 0..col3, into: line, do: "-" 

  end

  defp display_row({rec_num, crtd_at, title}, [col1w, col2w, col3w]) do 

    line = " " <> rec_num
    line = for _ <- 0..col1w - String.length(rec_num) - 1, into: line, do: " "
    line = line <> "|"

    line = line <> " " <> crtd_at
    line = for _ <- 0..col2w - String.length(crtd_at) - 1, into: line, do: " "
    line = line <> "|"

    line = line <> " " <> title
    line = for _ <- 0..col3w - String.length(title) - 1, into: line, do: " "
    # line = line <> "|"

  end  

  defp display_rec({rec_num, crtd_at, title}) do 
    IO.puts "#{rec_num}, #{crtd_at}, #{title}"
  end



  # ***************************************************************************

  def tbl_row_data(row_col_data, col_fmtrs) do 

    line_data = Enum.zip(row_col_data, col_fmtrs)
    line = Enum.reduce(line_data, <<>>, 
            fn (line_data, line) -> tbl_row_data_col(line_data, line) <> "|" end
            )
    String.rstrip(line, ?|);
  end
  def tbl_row_data_col({col_data, col_fmtr}, line) when col_fmtr > 0 do 
    line = line <> " " <> col_data
    line = for _ <- 0..col_fmtr - String.length(col_data) - 2, into: line, do: " "
  end


  def tbl_row_border(cols) do 
    line = Enum.reduce(cols, <<>>, 
            fn (col, line) -> tbl_row_border_col(col, line) <> "+" end
            )
    String.rstrip(line, ?+);
  end
  def tbl_row_border_col(col, line) when col > 0 do 
    line = for _ <- 0..col-1, into: line, do: "-" 
  end
  def tbl_row_border_col(col, line) when col <= 0 do 
    line
  end


  # ***************************************************************************
  
  # def build_fmtr(list_of_hash_dicts) do 
  #   {col1_mx, col2_mx, col3_mx} = get_max_cols_widths(list_of_hash_dicts)
  #   [col1_mx + 2, col2_mx + 2, col3_mx + 2]
  # end


  # def get_max_cols_widths(list_of_hash_dicts) do 
  #   Enum.reduce(list_of_hash_dicts, {0, 0, 0}, &get_max_cols_widths(&1, &2))
  # end

  # def get_max_cols_widths(hash_dict, {col1_mx, col2_mx, col3_mx}) do 

  #   {rec_num, crtd_at, title} = extract(hash_dict)

  #   new_col1_mx = max(col1_mx, String.length(rec_num))
  #   new_col2_mx = max(col2_mx, String.length(crtd_at))
  #   new_col3_mx = max(col3_mx, String.length(title))

  #   {new_col1_mx, new_col2_mx, new_col3_mx}

  # end


  def build_fmtr(list_of_hash_dicts) do 
    {col1_mx, col2_mx, col3_mx} = get_max_cols_widths(list_of_hash_dicts)
    [col1_mx + 2, col2_mx + 2, col3_mx + 2]
  end


  def get_max_cols_widths(list_of_hash_dicts) do 
    Enum.reduce(list_of_hash_dicts, {0, 0, 0}, &get_max_cols_widths(&1, &2))
  end

  def get_max_cols_widths(hash_dict, {col1_mx, col2_mx, col3_mx}) do 

    [rec_num, crtd_at, title] = extract(hash_dict)

    new_col1_mx = max(col1_mx, String.length(rec_num))
    new_col2_mx = max(col2_mx, String.length(crtd_at))
    new_col3_mx = max(col3_mx, String.length(title))

    {new_col1_mx, new_col2_mx, new_col3_mx}

  end


  # ***************************************************************************

  # def extract(record_dict) do
  #   rec_num = "#{HashDict.get(record_dict, "number")}"
  #   crtd_at = "#{HashDict.get(record_dict, "created_at")}"
  #   title = "#{HashDict.get(record_dict, "title")}"
  #   {rec_num, crtd_at, title}
  # end

  def extract(record_dict) do
    rec_num = "#{HashDict.get(record_dict, "number")}"
    crtd_at = "#{HashDict.get(record_dict, "created_at")}"
    title = "#{HashDict.get(record_dict, "title")}"
    [rec_num, crtd_at, title]
  end


end