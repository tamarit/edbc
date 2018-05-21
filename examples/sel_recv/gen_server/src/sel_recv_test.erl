-module(sel_recv_test).

-export([test/0]).

test() -> 
	sel_recv:start_link(),
	sel_recv:test(),
	sel_recv:stop(),
	ok.

