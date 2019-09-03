module asdf

import <std> (print)

main :: @() -> int
main:
	for i <- [1, 2, 3]:
		@print("i")
	#


	@print("x")
	if false:
		@print("Something true")
	#else:
		@print("Something false")
		// int i = 5;
		while true:
			// int i = 1;
			@print("asdf")
			// int x = @print("asdf")
		#
	#
	int i <- 5;
	while false:
		// int x = @print("asdf")
		// char c = 'x'
		// c = @print("i")
		// c = @print("i")
		i = i - 1
	#
	// char c <- @print("i")
	return 0
#

f :: int -> int -> int
f x y:
	return x * y
#

// voidFunc :: @string -> ()
// v s:
// 	@print(s)
// #
