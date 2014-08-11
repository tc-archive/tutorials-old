# Write a function prefix that takes a string. It should 
# return a new function that takes a second string. When that 
# second function is called, it will return a string 
# containing the first string, a space, and the second string.

prefixer_builder = 
	fn str ->
		fn str2 -> "#{str} #{str2}" end
	end

prefixer_builder.("Elixir").("Rocks"!)

greeter = prefixer_builder.("Hello,")
greeter.("Tim!")
greeter.("Nu!")


fareweller = prefixer_builder.("Goodbye,")
fareweller.("Tim!")
fareweller.("Nu!")


# Just messing around!

host_builder = 
	fn greeter, fareweller ->
		fn name -> 
			"#{greeter.(name)} #{fareweller.(name)}"
		end
	end

host = hostbuilder.(greeter, fareweller)
host.("Bob!")
