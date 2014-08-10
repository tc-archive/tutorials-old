defmodule Issues.TabularDataGenerator do

  @moduledesc """
  A module for generating strings relating to rows of tabular data.
  """

  # Defines the character used to draw border row (horizontal lines).
  @border_row Application.get_env(:display, :border_row, "-")
    # Defines the character used to draw border column (vertical lines).
  @border_col Application.get_env(:display, :border_col, "|")
    # Defines the character used to draw border row-column intersection points.
  @border_intersect Application.get_env(:display, :border_intersect, "+")
    # Defines the character used to 'pad' data cells.
  @cell_pad Application.get_env(:display, :cell_pad, " ")

  # ***************************************************************************

  @doc"""
  Return a string denoting a 'table border row' w.r.t the specified column 
  formatter.

  e.g.  fmtr    < [5,8,5]
        output  > "-----+--------+-----"

  """
  def tbl_row_border(fmtr) do 

    # Generate the ouput by reducing each col section into the empty binary.
    output = Enum.reduce(fmtr, <<>>, 
      fn (col_width, output) -> 
        tbl_row_border_col(col_width, output) <> @border_intersect end
      )

    # Trim the trailing intersector.
    << char :: utf8 >> = @border_intersect
    String.rstrip(output, char)

  end

  # Generate a single column section.
  defp tbl_row_border_col(col_width, output) when col_width > 0 do 
    for _ <- 0..col_width-1, into: output, do: @border_row 
  end

  # Handle the pointless case. Maybe we should let this error?
  defp tbl_row_border_col(col_width, output) when col_width <= 0 do 
    output
  end


  # ***************************************************************************

  @doc"""
  Return a string denoting a 'table border row' w.r.t the specified column 
  formatter and the data.

  e.g.  data    < [A, B, C]
        fmtr    < [5,8,5]
        output  > " A   | B      | C   "

  """
  def tbl_row_data(row_col_data, col_fmtrs) do 

    # Zip the data list and formater list together.
    # [So each piece of column data is paired with it's formater]
    line_data = Enum.zip(row_col_data, col_fmtrs)

    # Generate the ouput by reducing each col section into the empty binary.
    line = Enum.reduce(line_data, <<>>, 
      fn (line_data, line) -> 
        tbl_row_data_col(line_data, line) <> @border_col end
      )

    # Trim the trailing table border.
    << char :: utf8 >> = @border_col
    String.rstrip(line, char)

  end

  # Generate a single column section.
  defp tbl_row_data_col({col_data, col_fmtr}, line) when col_fmtr > 0 do 
    line = line <> @cell_pad <> col_data
    for _ <- 0..col_fmtr - String.length(col_data) - 2, into: line, do:  @cell_pad
  end


end