# Write a function that takes three arguments. If the first 
# two are zero, return “FizzBuzz”. If the first is zero, 
# return “Fizz”. If the second is zero return “Buzz”. 
# Otherwise return the third argument. 
# Do not use any language features that we haven’t yet 
# covered in this book.



# Parameter Solution
#
fizz_fn = fn
	0, 0, _c	-> "FizzBuzz"
	0, _b, _c 	-> "Fizz"
	_a, 0, _c 	-> "Buzz"
	_a, _b, c 	-> c
end


fizz_fn.(0, 0, 1)
fizz_fn.(0, 1, 1)
fizz_fn.(1, 0, 1)
fizz_fn.(1, 1, "Pancake!")



# Tuple Solution
#
buzz_fn = fn
	{0, 0, _c} 	-> "FizzBuzz"
	{0, _b, _c} -> "Fizz"
	{_a, 0, _c} -> "Buzz"
	{_a, _b, c} -> c
end


buzz_fn.({0, 0, 1})
buzz_fn.({0, 1, 1})
buzz_fn.({1, 0, 1})
buzz_fn.({1, 1, "Pancake!"})

