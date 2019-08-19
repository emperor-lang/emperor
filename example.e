module asdf

import <std> (print)

component ExampleComponent <: Countable, <: Semigroup:
	length :: [char] -> int
	length cs:
		return 0
##

component Semigroup:
	append :: [char] -> [int]
	append cs: return fmap(length, cs);#
#

class Countable <: Counter:
// 	length :: [char] -> int
#
i :: int
i:
	int x <- 2
	return x
#

myprint :: @[char] -> ()
myprint cs:
	@print(cs);
#

sum :: [int] -> int
sum is:
	int total = 0
	for i <- is:
		total = total + 1
	#
	return total
#

