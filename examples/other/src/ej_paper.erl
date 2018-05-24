-module(ej_paper).
-export([
        abs/1, 
        multiple_returns/2, 
        max/2,
        wrong_abs_wrong_post/1,
        wrong_abs_partial_right_post/1,
        wrong_abs_right_post/1,
        abs_neg/1,
        fib/1,
        find/2,
        binary_search/2,
        area/1,
        g1/0,
        g2/0,
        g3/0,
        g4/0,
        f_time/1,
        f_time2/1,
        qsort/1,
        qsort_tr/1,
        isort/1,
        sum_lol/1,
        di/1
    ]).

-include_lib("edbc.hrl").

% method Abs(x: int) returns (y: int)
%    ensures 0 <= y
% {
% ...
% }


abs(0) -> 
    0;
abs(N) -> 
    case N > 0 of 
        true -> 
            N;
        false -> 
            -N
    end.

?POST(fun() -> ?R >= 0 end).

% method MultipleReturns(x: int, y: int) returns (more: int, less: int)
%    requires 0 < y
%    ensures less < x
%    ensures x < more
% {
%    more := x + y;
%    less := x - y;
% }

% This call makes the postcondition fails when the precondition is not defined: 
% ej_paper:multiple_returns(3, -4).
% This is not failing without pre: 
% ej_paper:multiple_returns(3, 4).

?PRE(fun() -> ?P(2) > 0 end).

multiple_returns(X, Y) ->
    {X + Y, X - Y}.

?POST(
    fun() ->
        {M, L} = ?R, 
        L < ?P(1) andalso ?P(1) < M 
    end).

% method Max(a: int, b:int) returns (c: int)
%   ensures (c == a || c == b) && c >= a && c >= b;
% {
%   if (a > b)
%     { return a; }
%   else
%     { return b; }
% }

max(X, Y) ->
    case X > Y of 
        true ->
            X;
        false -> 
            Y 
    end.

?POST(
    fun() -> 
            (?R == ?P(1) orelse ?R == ?P(2)) 
        andalso
            (?R >= ?P(1) andalso ?R >= ?P(2)) 
    end).


% method Abs(x: int) returns (y: int)
%   ensures 0 <= y
% {
%   y := 0;
% }

wrong_abs_wrong_post(N) -> 
    0.

?POST(fun() -> ?R >= 0 end).


% method Abs(x: int) returns (y: int)
%   ensures 0 <= y
%   ensures 0 <= x ==> x == y
% {
%   y := 0;
% }

wrong_abs_partial_right_post(N) -> 
    0.

?POST(
    fun() -> 
            (?R >= 0) 
        andalso 
            (?P(1) < 0 orelse ?P(1) == ?R) 
    end).

% method Abs(x: int) returns (y: int)
%   ensures 0 <= y
%   ensures 0 <= x ==> x == y
%   ensures x < 0 ==> y == -x
% {
%   y := 0;
% }

wrong_abs_right_post(N) -> 
    0.

?POST(
    fun() -> 
            (?R >= 0) 
        andalso 
            (?R == ?P(1) orelse ?R == -?P(1)) 
    end).


% method Abs(x: int) returns (y: int)
%    requires x < 0
% {
% ...
% }

?PRE(fun() -> ?P(1) < 0 end).

abs_neg(N) -> 
    -N.

?POST(fun() -> ?R > 0 andalso  ?R == -?P(1) end).

% function fib(n: nat): nat
% decreases n
% {
%    if n == 0 then 0 else
%    if n == 1 then 1 else
%         fib(n - 1) + fib(n - 2)
% }

?PRE(fun() -> ?P(1) >= 0 end).
?DECREASES(?P(1)).

% To see how it fails just remove the clause for number 1.

fib(0) -> 
    0;
fib(1) -> 
    1;
fib(N) ->
    fib(N - 1) +  fib(N - 2).


% method Find(a: array<int>, key: int) returns (index: int)
%    requires a != null
%    ensures 0 <= index ==> index < a.Length && a[index] == key
%    ensures index < 0 ==> forall k :: 0 <= k < a.Length ==> a[k] != key
% {
%    // Open in editor for a challenge...
% }

?PRE(fun() -> length(?P(1)) > 0 end).

find(L, K) ->
    find(L, K, 1).

?POST(
    fun() -> 
            ?R < 0 
        orelse  
            (?R < length(?P(1)) andalso lists:nth(?R, ?P(1)) == ?P(2)) 
    end).
?POST(
    fun() -> 
            ?R > 0 
        orelse 
            lists:all(fun(K) -> K /= ?P(2) end, ?P(1)) 
    end).
    
% Actual find function. 

find([], _, _) -> 
    -1;
find([K | T], K, N) -> 
    N;
find([_ | T], K, N) -> 
    find(T, K, N + 1).


% method BinarySearch(a: array<int>, key: int) returns (index: int)
%    requires a != null && sorted(a)
%    ensures ...
% {
% ...
% }


?PRE(
    fun() -> 
            length(?P(1)) > 0
        andalso 
            lists:sort(?P(1)) == ?P(1) 
    end).

% https://gist.github.com/Janiczek/3133037

binary_search(List, N) ->
  Length = length(List),
  Middle = (Length + 1) div 2, %% saves us hassle with odd/even indexes

  case Middle of
    0 -> 
        -1; %% empty list -> item not found
    _ -> 
   
      Item = lists:nth(Middle, List),
      
      case Item of
        N -> 
            Middle; %% yay, found it!
        _ -> 
            case Item > N of
                true  -> 
                    binary_search(
                        lists:sublist(List, Length - Middle), N); %% LT, search on left side
                false -> 
                    binary_search(
                        lists:nthtail(Middle, List), N)           %% GT, search on right side
            end
      end
  end.


?POST(
    fun() -> 
            ?R < 0 
        orelse  
            (?R < length(?P(1)) andalso lists:nth(?R, ?P(1)) == ?P(2)) 
    end).
?POST(
    fun() -> 
            ?R > 0 
        orelse 
            lists:all(fun(K) -> K /= ?P(2) end, ?P(1)) 
    end).

?PRE(
    fun() -> 
        case ?P(1) of 
            {square, Side}  when is_integer(Side) ->
                true;
            {circle, Radius} when is_number(Radius) ->
                true;
            {triangle, A, B, C} ->
                true;
            _ ->
                {false, "The figure is not valid."}
        end
    end).
?PURE.

area({square, Side}) when is_integer(Side) ->
    Side * Side;
area({circle, Radius}) when is_number(Radius) -> 
    % io:format("Radius: ~p\n", [Radius]),
    3.14 * Radius * Radius; %% well, almost
area({triangle, A, B, C}) ->
      S = (A + B + C) / 2,
      math:sqrt(S * (S-A) * (S-B) * (S-C)).


%% A higher order function which depends on its first argument.
fold(_Fun, Acc, []) -> 
    Acc;
fold(Fun, Acc, [H|T]) -> 
    fold(Fun, Fun(H, Acc), T).
%% A pure closure is passed to a higher order function
%% so function g1/0 will be determined pure by the analysis. 
?PURE.
g1() -> 
    fold(fun erlang:'*'/2, 1, [2, 3, 7]).
%% An impure closure is passed to a higher order function 
%% so function g2/0 is classified as impure.
?PURE.
g2() -> 
    fold(fun erlang:put/2, computer, [ok, error]).

%% One level of indirection: it is not apparent this is a higher 
%% order function since no direct call to its argument is made. 
fold1(Fun, Acc, Lst) ->
  fold(Fun, Acc, Lst).
%% Two levels of indirection. The function argument has also 
%% changed position.
fold2(Lst, Fun) ->
  fold1(Fun, 1, Lst).

?PURE.
g3() -> 
    fold1(fun erlang:put/2, ok, [computer, error]).

?PURE.
g4() -> 
    fold2([2, 3, 7], fun erlang:'*'/2).

% Sample of failing predicate
% ?EXPECTED_TIME(fun() -> length(?P(1)) * 50 end).
% Sample of correct predicate
?EXPECTED_TIME(fun() -> 20 + (length(?P(1)) * 100) end).
% Sample of predicate timeouting
?TIMEOUT(fun() -> length(?P(1)) * 50 end).
% Sample of predicate no timeouting
% ?TIMEOUT(fun() -> 20 + (length(?P(1)) * 100) end).
% -spec f_time(list(any()))  -> list(any()).
% -spec f_time(integer())  -> integer().
% Sample call
% ej_paper:f_time(lists:seq(1,10)).
f_time(L) -> 
    [timer:sleep(100) || _ <- L].


% This constract is forgetting the odd tasks.
% ?EXPECTED_TIME(fun() -> 20 + (length(?P(1)) * 100) end).
% This constract is the good one.
?EXPECTED_TIME(fun() -> 20 + ((length(?P(1)) * 300) / 2) end).

f_time2(L) ->
    [f_time2_run(E) || E <- L].

f_time2_run(N) when (N rem 2) == 0 -> 
    timer:sleep(100);
f_time2_run(N) when (N rem 2) /= 0 -> 
    timer:sleep(200).

% Quick sort
% http://erlangexamples.com/tag/quicksort/

% ?EXPECTED_TIME(fun() -> length(?P(1)) * math:log2(length(?P(1))) * 10 end).
?EXPECTED_TIME(fun() -> length(?P(1)) * 10 end).
% ?TIMEOUT(fun() -> (length(?P(1)) * 1) end).

qsort(L) -> 
    qsort_aux(L).

qsort_aux([]) -> 
    timer:sleep(5),
    [];
qsort_aux([Pivot|T]) ->
       timer:sleep(5),
       qsort_aux([X || X <- T, begin timer:sleep(5), true end, X < Pivot])
       ++ [Pivot] ++
       qsort_aux([X || X <- T, begin timer:sleep(5), true end, X >= Pivot]).

%% Quick Sort (tail recursive version)
%% Think thru, you'll see it very easy to understand :p
% http://erlangexamples.com/tag/quicksort/


% ?EXPECTED_TIME(fun() -> length(?P(1)) * length(?P(1)) * 0.0000075 end).
?EXPECTED_TIME(fun() -> length(?P(1)) * math:log2(length(?P(1))) * 0.0000075 end).

qsort_tr(L) ->
    qsort_aux_tr(L).

qsort_aux_tr([]) -> [];
qsort_aux_tr([Single]) -> [Single];
qsort_aux_tr([Pivot|Rest]) ->
    {Smallers, Greaters} = qsort(Pivot, Rest),
    SortedSmallers = qsort_aux_tr(Smallers),
    SortedGreaters = qsort_aux_tr(Greaters),
    SortedSmallers ++ [Pivot] ++ SortedGreaters.

qsort(Pivot, List) -> qsort(Pivot, [], [], List).

qsort(_Pivot, Smallers, Greaters, []) -> {Smallers, Greaters};
qsort(Pivot, Smallers, Greaters, [First|Rest]) when First < Pivot ->
    qsort(Pivot, [First|Smallers], Greaters, Rest);
qsort(Pivot, Smallers, Greaters, [First|Rest]) when First >= Pivot ->
    qsort(Pivot, Smallers, [First|Greaters], Rest).


?EXPECTED_TIME(fun() -> length(?P(1)) * 10 end).

isort(L) -> 
    lists:foldl(fun insert/2, [], L).
 
insert(X,[]) ->
    timer:sleep(10),
    [X];
insert(X,L=[H|_]) when X =< H -> 
    timer:sleep(10),
    [X|L];
insert(X,[H|T]) -> 
    timer:sleep(10),
    [H|insert(X, T)].

-define(TIME, 10).

?EXPECTED_TIME(fun() -> 10 + length(?P(1)) * length(?P(1)) * (?TIME + 1) end).
% ?EXPECTED_TIME(fun() -> length(?P(1)) * (?TIME + 1) end).

% Sample call (Expected time 10 * 10 * ?TIME)
% ej_paper:sum_lol([lists:seq(1,10) || _ <- lists:seq(1,10)]).

sum_lol(L) ->
    lists:foldl(
        fun(EL, AccOut) -> 
            lists:foldl(
                fun(E, AccIn) ->
                    timer:sleep(?TIME),
                    E + AccIn
                end,
                AccOut,
                EL)
        end,
        0,
        L).


?EXPECTED_TIME(fun() -> length(?P(1)) * length(?P(1)) * (?TIME + 1) end).
% ?EXPECTED_TIME(fun() -> length(?P(1)) * ?TIME  end).

% Sample call (Expected time 10 * 10 * ?TIME)
% ej_paper:di(lists:seq(1,10)).

di(L) ->
    lists:map(
        fun(_) -> 
            lists:map(
                fun(_) ->
                    timer:sleep(?TIME)
                end,
                L)
        end,
        L).




