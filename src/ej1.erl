-module(ej1).
-export([f/1, g/1, h/2, i/2]).

-include_lib("edbc.hrl").

?PRE(fun pre_f/0).
f(0) -> 1;
f(N) -> 
    Prev = f(N-1),
	% ?PRE_I(fun pre_f_i/1, [N]),
	Prev * 3.
?POST(fun post_f/0).

?PRE(fun pre_g/0).
g(1) -> 2.

?PRE(fun pre_h/0).
h(X, Y) -> 
	X / Y.

i(Elem, List) -> 
    [Elem,1 | List].
?POST(fun post_i/0).

-none(1).
pre_f() -> 
	?P(1) >= 0.

post_f() -> 
    io:format("f(~p) = ~p\n", [?P(1), ?R()]),
	?R() >= ?P(1).

pre_g() -> 
	?P(1) == 1.

pre_h() -> 
	C1 = 
        ?P(1) /= ?P(2),
	C2 = 
        ?P(2) /= 0,
	C1 and C2.

post_i() -> 
    (length(?P(2)) + 1) == length(?R()).

% pre_f_i(N) -> 
%     Prev > N,
%     true.