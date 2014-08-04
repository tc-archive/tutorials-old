# Go into iex. Create and run the functions that do the following

# – list_concat.([1,2,3], [4,5,6]) #=> [1,2,3,4,5,6] 
list_concat = fn (l1, l2) -> l1 ++ l2 end
list_concat.([1], [2, 3])

# – sum.(1, 2, 3) #=> 6
sum = fn a, b, c -> a + b + c end
sum.(1, 2, 3)

# – pair_tuple_to_list.( { 8, 7 } ) #=> [ 8, 7 ]
pair_tuple_to_list = fn {a, b} -> [a, b] end
pair_tuple_to_list.({1, 2})
