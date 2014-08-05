# *************************************************************************************************
#    
#  - Rewrite the FizzBuzz example using case.
#
#  - We now have three different implementations of FizzBuzz. One uses cond, one uses case, and one 
#    uses separate functions with guard clauses. 
# 
#    Take a minute to look at all three. Which do you feel best expresses the problem. Which will 
#    be easiest to maintain? The case style and the one using guard clauses are somewhat different 
#    to control structures in most other languages. If you feel that one of these was the best 
#    implementation, can you think of ways of reminding yourself to investigate these options as you 
#    write more Elixir code in the future?
#
#  - Many built-in functions have two forms. The xxx form returns the tuple {:ok, data} and the 
#    xxx! form returns data on success but raises an exception otherwise. However, there are some 
#    functions that don’t have the xxx! form.
#    
#    Write a function ok! that takes an arbitary parameter. If the parameter is the tuple 
#    {:ok, data} return the data. Otherwise raise an exception containing information from the 
#    parameter.
#
#    You could use your function like this:
#
#    file = ok! File.open("somefile")
#
#


➤ Exercise: ControlFlow-3
Many built-in functions have two forms. The xxx form returns the tuple {:ok, data} and the xxx! form returns data on success but raises an exception otherwise. However, there are some functions that don’t have the xxx! form.
Write a function ok! that takes an arbitary parameter. If the parameter is the tuple {:ok, data} return the data. Otherwise raise an exception containing information from the parameter.
You could use your function like this:
file = ok! File.open("somefile")
#
defmodule StringIt2 do 



end


