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

    
    
    Enum.each(list_of_hash_dicts, fn hash_dict -> display_hash_dict(hash_dict) end)

  end

  def build_fmtr(list_of_hash_dicts) do 
    Enum.reduce(list_of_hash_dicts, {0, 0, 0}, &build_fmtr(&1, &2))
  end

  def build_fmtr(hash_dict, {mx_rec_num_wth, mx_crtd_at_wth, mx_title_wth}) do 

    {rec_num, crtd_at, title} = extract(hash_dict)

    new_mx_rec_num_wth = max(mx_rec_num_wth, String.length(rec_num))
    new_mx_crtd_at_wth = max(mx_crtd_at_wth, String.length(crtd_at))
    new_mx_title_wth = max(mx_title_wth, String.length(title))

    {new_mx_rec_num_wth, new_mx_crtd_at_wth, new_mx_title_wth}
  end


  defp display_hash_dict(hash_dict) do 

    hash_dict 
      |> extract
      |> display_rec

  end


  defp display_rec({rec_num, crtd_at, title}) do 
    IO.puts "#{rec_num}, #{crtd_at}, #{title}"
  end


  def extract(record_dict) do
    rec_num = "#{HashDict.get(record_dict, "number")}"
    crtd_at = "#{HashDict.get(record_dict, "created_at")}"
    title = "#{HashDict.get(record_dict, "title")}"
    {rec_num, crtd_at, title}
  end


end