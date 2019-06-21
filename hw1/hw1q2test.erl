-module(hw1q2test).
-export([runtest/0]).

assertEquals(Func, Args, ExpectedResult) ->
    Result = apply(Func, Args),
    case Result == ExpectedResult of
        true -> ok;
        false -> throw(io:format("Assertion failed! ~p(~p) = ~p != ~p", [Func, Args, Result, ExpectedResult]))
    end.

assertThrows(Func, Args) ->
    try apply(Func, Args) of
        _Val -> throw(io:format("Expected ~p(~p) to throw, but it didn't throw", [Func, Args]))
    catch
        error:_X -> ok
    end.

runtest() -> 
    assertEquals(fun game:canWin/1, [0], false),
    assertEquals(fun game:canWin/1, [1], true),
    assertEquals(fun game:canWin/1, [2], true),
    assertEquals(fun game:canWin/1, [3], false),
    assertEquals(fun game:canWin/1, [4], true),
    assertEquals(fun game:canWin/1, [5], true),
    assertEquals(fun game:canWin/1, [6], false),
    assertEquals(fun game:canWin/1, [7], true),
    assertEquals(fun game:canWin/1, [8], true),
    assertEquals(fun game:canWin/1, [9], false),

    assertThrows(fun game:canWin/1, [-1]),
    assertThrows(fun game:canWin/1, [-3]),
    assertThrows(fun game:canWin/1, [1.5]),
    assertThrows(fun game:canWin/1, [3.5]),
    assertThrows(fun game:canWin/1, [blah]),
    assertThrows(fun game:canWin/1, ["blah"]),

    assertThrows(fun game:nextMove/1, [-1]),
    assertEquals(fun game:nextMove/1, [0], false),
    assertEquals(fun game:nextMove/1, [1], {true, 1}),
    assertEquals(fun game:nextMove/1, [2], {true, 2}),
    assertEquals(fun game:nextMove/1, [3], false),
    assertEquals(fun game:nextMove/1, [4], {true, 1}),
    assertEquals(fun game:nextMove/1, [5], {true, 2}),
    assertEquals(fun game:nextMove/1, [6], false),
    assertEquals(fun game:nextMove/1, [7], {true, 1}),
    assertEquals(fun game:nextMove/1, [8], {true, 2}),
    assertEquals(fun game:nextMove/1, [9], false),

    assertThrows(fun game:nextMove/1, [-1]),
    assertThrows(fun game:nextMove/1, [-3]),
    assertThrows(fun game:nextMove/1, [1.5]),
    assertThrows(fun game:nextMove/1, [3.5]),
    assertThrows(fun game:nextMove/1, [blah]),
    assertThrows(fun game:nextMove/1, ["blah"]).