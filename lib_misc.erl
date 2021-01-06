-module(lib_misc).
-export([temp_convert/0,
         mult/1,
         for/3,
         map1/2, map2/2,
         qsort/1, pythag/1,
         perms/1,
         max/2,
         filter/2, filter2/2,
         odds_and_evens1/1,
         odds_and_evens2/1
]).

%% higher order functions
temp_convert() -> fun({c, C}) -> {f, 32 + C*9/5};
                     ({f, F}) -> {c, (F-32)*5/9}
                  end.

mult(Times) -> fun(X) -> X * Times end.

%% control abstractions
for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I) | for(I+1, Max, F)].

%% map using pattern matching
map1(_, [])    -> [];
map1(F, [H|T]) -> [F(H) | map1(F, T)].

%% map using list comprehension
map2(F, L) -> [F(X) || X <- L].

%% quick sort
%% inefficient by using ++
qsort([]) -> [];
qsort([Pivot|T]) -> qsort([X || X <- T, X < Pivot])
                    ++ [Pivot] ++
                    qsort([X || X <- T, X >= Pivot]).

%% pythagorean triplets using list comprehension
pythag(N) ->
    [ {A, B, C} ||
        A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        A*A + B*B =:= C*C,
        A + B + C =< N
    ].

%% string permutation using list comprehension
perms([]) -> [[]];
perms(L)  -> [ [H|T] || H <- L, T <- perms(L--[H]) ].

%% guards
max(X, Y) when X > Y -> X;
max(_, Y) -> Y.

filter(P, [H|T]) ->
    case P(H) of
        true  -> [H | filter(P, T)];
        false -> [ filter(P, T)]
    end;
filter(_, []) -> [].

%% filter using pattern matching
filter2(P, [H|T]) -> filter3(P(H), H, P, T);
filter2(_, [])    -> [].

filter3(true, H, P, T)  -> [H | filter2(P, T)];
filter3(false, _, P, T) -> filter2(P, T).


%% the following functions are
%% to demonstrate accumulators

%% this version traverses the list twice
odds_and_evens1(L) ->
    Odds = [X || X <- L, X =:= 1],
    Evens = [X || X <- L, X =:= 0],
    {Odds, Evens}.

%% by using accumulators,
%% L will only be traversed once
%% the lists will be built up in natural order
%% new elements will be added to list head.
odds_and_evens2(L) -> odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 -> odds_and_evens_acc(T, [H|Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) -> {Odds, Evens}.
