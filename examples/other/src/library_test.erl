-module(library_test).

-export([test1/0]).

test1() -> 
	library:start(),
	library:checkout(a, l1),
	library:checkout(b, l2),
	library:checkout(a, l1),
	library:lookup(l1),
	library:lookup(l2),
	library:lookup(l3),
	library:return(l1),
	library:return(l2),
	library:return(l1),
	library:return(l3),
	library:stop().