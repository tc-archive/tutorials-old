# The operator rem(a, b) returns the remainder after dividing a 
# by b. Write a function that takes a single integer (n) and 
# which calls the function in the previous exercise, passing 
# it rem(n,3), rem(n,5), and n. Call it 7 times with the 
# arguments 10, 11, 12, etc. You should get “Buzz, 11, Fizz, 
# 13, 14, FizzBuzz, 16”.


# Parameter Solution
#
fizz_fn = fn
	0, 0, _c	-> "FizzBuzz"
	0, _b, _c 	-> "Fizz"
	_a, 0, _c 	-> "Buzz"
	_a, _b, c 	-> c
end


fizz_rem_fn = fn 
	n -> fizz_fn.({rem(n, 3), rem(n, 5), n})
end

fizz_rem_fn.(10)
fizz_rem_fn.(11)
fizz_rem_fn.(12)
fizz_rem_fn.(13)
fizz_rem_fn.(14)
fizz_rem_fn.(15)
fizz_rem_fn.(16)


# Tuple Solution
#
buzz_fn = fn
	{0, 0, _c} 	-> "FizzBuzz"
	{0, _b, _c} -> "Fizz"
	{_a, 0, _c} -> "Buzz"
	{_a, _b, c} -> c
end


buzz_rem_fn = fn 
	n -> buzz_fn.({rem(n, 3), rem(n, 5), n})
end

buzz_rem_fn.(10)
buzz_rem_fn.(11)
buzz_rem_fn.(12)
buzz_rem_fn.(13)
buzz_rem_fn.(14)
buzz_rem_fn.(15)
buzz_rem_fn.(16)