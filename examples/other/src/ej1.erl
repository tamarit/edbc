-module(ej1).
-export([f/1, g/1, h/2, i/2, f_rec/2, f_time/1, f_pure/0, f_type/2, start/0]).

-include_lib("edbc.hrl").


?PRE(fun pre_f/0).
f(0) -> 1;
f(N) -> 
    Prev = f(N-1),
    % ?assert(pre_f_i(Prev, N)),
    % Bound variables are sent
	% ?PRE_I(fun pre_f_i/0),
    % Bound variables are not sent (for reusable contracts)
    % ?PRE_I(fun pre_f_i/2. [Prev, N ]),
	Prev * 3.
?POST(fun post_f/0).

% ?PRE(fun pre_g/0).
?PRE(fun() -> (?P(1) == 1) orelse (?P(1) == 2) end).
g(1) -> 2;
g(2) -> 3.
?POST(fun() -> (?R == 2) orelse (?R == 3) end).

?PRE(fun pre_h/0).
h(X, Y) -> 
	X / Y.

i(Elem, List) -> 
    [Elem,1 | List].
?POST(fun post_i/0).

pre_f() -> 
    case ?P(1) >= 0 of 
        true -> 
            true;
        false -> 
	        {
                false, 
                "The first parameter should be greater than or equal to 0."
            }
    end.

post_f() -> 
    io:format("f(~p) = ~p\n", [?P(1), ?R]),
	?R >= ?P(1).

% pre_g() -> 
% 	(?P(1) == 1) orelse (?P(1) == 2).

pre_h() -> 
	C1 = 
        ?P(1) /= ?P(2),
	C2 = 
        ?P(2) /= 0,
	C1 and C2.

post_i() -> 
    (length(?P(2)) + 1) == length(?R).


% ?DECREASES([?P(1), ?P(2)]).
?DECREASES(?P(2)).
?PRE(fun() -> ?P(1) < ?P(2) end).

%% @doc This is a <em>very</em> useful function.

% Example of a failing call
% ej1:f_rec(1,4). 
f_rec(M, N) -> 
    io:format("{M, N}: ~p\n", [{M, N}]),
    case M of 
        0 -> 
            f_rec(M + 1, N + 2);
        N -> 
            f_rec(M - 1, N - 1);
        _ ->
            case N of 
                0 -> 
                    M;
                _ -> 
                    f_rec(M, N - 1)
            end
    end.

%% @doc This is a <em>very</em> useful function.
%%  
%% 
%% Intersting
%% Function


% Sample of failing predicate
?EXPECTED_TIME(fun() -> length(?P(1)) * 50 end).
% Sample of correct predicate
% ?EXPECTED_TIME(fun() -> 20 + (length(?P(1)) * 100) end).
% Sample of predicate timeouting
% ?TIMEOUT(fun() -> length(?P(1)) end).
% Sample of predicate no timeouting
?TIMEOUT(fun() -> 20 + (length(?P(1)) * 100) end).
% -spec f_time(list(any()))  -> list(any()).
-spec f_time(integer())  -> integer().


% Sample call
% ej1:f_time(lists:seq(1,10)).
f_time(L) -> 
    [timer:sleep(100) || _ <- L].


%% @doc This is a <em>very</em> useful function.

?PURE.
f_pure() -> 
    % Detected side-effect operations
    % os:cmd("ls"),
    % io:format("I'm not pure"),
    % spawn(fun() -> ok end),
    % self()!hi,
    % dets:open_file(table, []),
    ets:new(table, [set]),
    % rand:uniform(30),
    % put(r, 1),
    % Not-detected side-effect operations
    % get(r),
    % This exits are reported as they are (they are not affected by the tracing). However, the tracestack is lost.
    % 3/0,
    % exit("out"),
    % throw("out"),
    ok.

% -spec f_type(list(integer()))  -> list(atom()).
-spec f_type([integer()], integer())  -> [integer()].
%% @doc This is a <em>very</em> useful function.

f_type(L, X) -> 
    % case sheriff:check(L, "list(integer())") of 
    %     true -> 
    %         [timer:sleep(100) || _ <- L];
    %     false -> 
    %         {error, badarg}
    % end.
    [length(L) + X | L].

-spec start() -> atom().
start() ->
    ok.

% pre_f_i(Prev, N = 2) -> 
%     io:format("Prev: ~p\n", [Prev]),
%     Prev > N.

% pre_f_i(Prev, N) -> 
%     io:format("Prev: ~p\n", [Prev]),
%     Prev > N.

% pre_f_i() -> 
%     Prev > N,
%     true.