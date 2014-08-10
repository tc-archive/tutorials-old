defmodule Issues.ConsoleDisplay do

  @moduledesc """
  """

  @border_row Application.get_env(:display, :border_row, "-")
  @border_col Application.get_env(:display, :border_col, "|")
  @border_intersect Application.get_env(:display, :border_intersect, "+")
  @cell_pad Application.get_env(:display, :cell_pad, " ")

  # ***************************************************************************


  def tbl_row_border(cols) do 
    line = Enum.reduce(cols, <<>>, 
      fn (col, line) -> tbl_row_border_col(col, line) <> @border_intersect end
      )
    << char :: utf8 >> = @border_intersect
    String.rstrip(line, char);
  end
  def tbl_row_border_col(col, line) when col > 0 do 
    for _ <- 0..col-1, into: line, do: @border_row 
  end
  def tbl_row_border_col(col, line) when col <= 0 do 
    line
  end



  def tbl_row_data(row_col_data, col_fmtrs) do 
    line_data = Enum.zip(row_col_data, col_fmtrs)
    line = Enum.reduce(line_data, <<>>, 
      fn (line_data, line) -> tbl_row_data_col(line_data, line) <> @border_col end
      )
    << char :: utf8 >> = @border_col
    String.rstrip(line, char);
  end
  defp tbl_row_data_col({col_data, col_fmtr}, line) when col_fmtr > 0 do 
    line = line <> @cell_pad <> col_data
    for _ <- 0..col_fmtr - String.length(col_data) - 2, into: line, do:  @cell_pad
  end


end