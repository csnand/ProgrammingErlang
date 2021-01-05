-module(geometry).
-export([area/1, test/0]).

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})           -> 3.14159 * Radius * Radius;
area({square, Side})             -> Side * Side.

test() ->
    12 = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    50.26544 = area({circle, 4}),
    test_worked.

