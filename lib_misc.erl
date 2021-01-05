-module(lib_misc).
-export([temp_convert/0, mult/1, for/3, map1/2, map2/2, qsort/1]).

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
