defmodule TabularDataGeneratorTest do

  use ExUnit.Case

  import Issues.TabularDataGenerator, only: [
    tbl_row_border: 1,
    tbl_row_data: 2,
    ]

  # ***************************************************************************
  
  # Tests!
  
  test "Correct table row border returned for column formatter. Good input" do
    assert tbl_row_border([1,1,1]) == "-+-+-"
    assert tbl_row_border([1,1,1,1,1]) == "-+-+-+-+-"
    assert tbl_row_border([5,8,5]) == "-----+--------+-----"
    assert tbl_row_border([2, 5, 3, 2, 1,6]) == "--+-----+---+--+-+------"
  end


  test "Correct table row border returned for column formatter. Bad input" do
    assert tbl_row_border([0]) == ""
    assert tbl_row_border([-1]) == ""
    assert tbl_row_border([0,0,0]) == ""
    assert tbl_row_border([0,5,0]) == "+-----"

  end

end