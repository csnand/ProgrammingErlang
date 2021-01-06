-module(error_handling).
-export([generate_exception/1,
         try_demo/0, catcher/1,
         catch_demo/0
]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

try_demo() ->
    [ catcher(I) || I <- lists:seq(1,5) ].

catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X  -> {N, caught, exited, X};
        error:X -> {N, caught, error,  X}
    end.

catch_demo() ->
        [ {I, catch generate_exception(I)} || I <- lists:seq(1,5) ].

