-module(ej1).
-export([f/1, g/1, h/2]).

-include_lib("edbc.hrl").


?PRE(fun pre_f/0).
f(0) -> 1;
f(N) -> 
	% ?PRE_I(fun pre_f/0),
	f(N-1) * 2.
?POST(fun post_f/0).

?PRE(fun pre_g/0).
g(1) -> 1.

?PRE(fun pre_h/0).
h(X, Y) -> 
	X / Y.

pre_f() -> 
	?P(1) >= 0.

post_f() -> 
	?R() =< ?P(1).

pre_g() -> 
	?P(1) == 1.

pre_h() -> 
	C1 = ?P(1) /= ?P(2),
	C2 = ?P(2) /= 0,
	C1 and C2.