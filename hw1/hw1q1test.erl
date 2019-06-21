-module(hw1q1test).
-export([runtest/0]).

validRectangle1() -> {rectangle,{dim,1,2}}.		% size 2
validRectangle3() -> {rectangle,{dim,5,5}}.		% size 25
validRectangle4() -> {rectangle,{dim,1,1}}.		% size 1

validTriangle2() -> {triangle,{dim,3,2}}.		% size 3
validTriangle3() -> {triangle,{dim,4,4}}.		% size 8

validEllipse1() -> {ellipse,{radius,1,2}}.      % size 2*pi
validEllipse2() -> {ellipse,{radius,2,2}}.		% size 4*pi	
validEllipse3() -> {ellipse,{radius,1,1}}.	    % size pi

%---valid structs
validShapes1() -> {shapes,[validEllipse1(), validEllipse1(), validRectangle1(), validTriangle2(), validRectangle3() ]}.
validShapes2() -> {shapes,[validEllipse2(),validRectangle1(), validTriangle2() , validTriangle3(), validRectangle3(), validRectangle4()]}.

invalidRectangle1() -> {rectangle, {dim, -1, -1}}.
invalidRectangle2() -> {rectangle, {dim, 1, -2}}.
invalidRectangle3() -> {rectangle, {radius, 1, 2}}.
invalidRectangle4() -> {rectangle, {blah, 1, 2}}.
invalidRectangle5() -> {rectangle, {dim, 1}}.

invalidTriangle1() -> {triangle, {dim, 1, -2}}.
invalidTriangle2() -> {triangle, {radius, 1, 2}}.
invalidTriangle3() -> {triangle, {blah, 1, 2}}.
invalidTriangle4() -> {triangle, {dim, 1}}.

invalidEllipse1() -> {ellipse, {radius, 1, -2}}.
invalidEllipse2() -> {ellipse, {dim, 1, 2}}.
invalidEllipse3() -> {ellipse, {blah, 1, 2}}.
invalidEllipse4() -> {ellipse, {radius, 1}}.

invalidShapes1() -> {blah, [validRectangle1(), validTriangle2(), validEllipse3()]}.
invalidShapes2() -> {shapes, [invalidRectangle1(), validTriangle2(), validEllipse3()]}.
invalidShapes3() -> {shapes, [invalidRectangle2(), validTriangle2(), validEllipse3()]}.
invalidShapes4() -> {shapes, [invalidRectangle3(), validTriangle2(), validEllipse3()]}.
invalidShapes5() -> {shapes, [invalidRectangle4(), validTriangle2(), validEllipse3()]}.
invalidShapes6() -> {shapes, [invalidRectangle5(), validTriangle2(), validEllipse3()]}.
invalidShapes7() -> {shapes, [validRectangle1(), invalidTriangle1(), validEllipse3()]}.
invalidShapes8() -> {shapes, [validRectangle1(), invalidTriangle2(), validEllipse3()]}.
invalidShapes9() -> {shapes, [validRectangle1(), invalidTriangle3(), validEllipse3()]}.
invalidShapes10() -> {shapes, [validRectangle1(), invalidTriangle4(), validEllipse3()]}.
invalidShapes11() -> {shapes, [validRectangle1(), validTriangle2(), invalidEllipse1()]}.
invalidShapes12() -> {shapes, [validRectangle1(), validTriangle2(), invalidEllipse2()]}.
invalidShapes13() -> {shapes, [validRectangle1(), validTriangle2(), invalidEllipse3()]}.
invalidShapes14() -> {shapes, [validRectangle1(), validTriangle2(), invalidEllipse4()]}.



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

assertThrowsOnAll(_Func, []) -> true;
assertThrowsOnAll(Func, [Arg | NextArgs]) -> 
    assertThrows(Func, [Arg]),
    assertThrowsOnAll(Func, NextArgs).

runtest() ->
    AllInvalidShapes = [
        invalidShapes1(),
        invalidShapes2(),
        invalidShapes3(),
        invalidShapes4(),
        invalidShapes5(),
        invalidShapes6(),
        invalidShapes7(),
        invalidShapes8(),
        invalidShapes9(),
        invalidShapes10(),
        invalidShapes11(),
        invalidShapes12(),
        invalidShapes13(),
        invalidShapes14()
    ],

    assertEquals(fun shapes:shapesArea/1, [validShapes1()], 42.56637061435917),
    assertEquals(fun shapes:shapesArea/1, [validShapes2()], 51.56637061435917),
    assertThrowsOnAll(fun shapes:shapesArea/1, AllInvalidShapes),

    assertEquals(fun shapes:squaresArea/1, [validShapes1()], 25),
    assertEquals(fun shapes:squaresArea/1, [validShapes2()], 26),
    assertThrowsOnAll(fun shapes:shapesArea/1, AllInvalidShapes),

    assertEquals(fun shapes:trianglesArea/1, [validShapes1()], 3),
    assertEquals(fun shapes:trianglesArea/1, [validShapes2()], 11),
    assertThrowsOnAll(fun shapes:shapesArea/1, AllInvalidShapes),

    RectangleFilter = shapes:shapesFilter(rectangle),
    assertEquals(RectangleFilter, [validShapes1()], {shapes, [validRectangle1(), validRectangle3()]}),
    assertEquals(RectangleFilter, [validShapes2()], {shapes, [validRectangle1(), validRectangle3(), validRectangle4()]}),
    assertThrowsOnAll(RectangleFilter, AllInvalidShapes),

    TriangleFilter = shapes:shapesFilter(triangle),
    assertEquals(TriangleFilter, [validShapes1()], {shapes, [validTriangle2()]}),
    assertEquals(TriangleFilter, [validShapes2()], {shapes, [validTriangle2(), validTriangle3()]}),
    assertThrowsOnAll(TriangleFilter, AllInvalidShapes),

    EllipseFilter = shapes:shapesFilter(ellipse),
    assertEquals(EllipseFilter, [validShapes1()], {shapes, [validEllipse1(), validEllipse1()]}),
    assertEquals(EllipseFilter, [validShapes2()], {shapes, [validEllipse2()]}),
    assertThrowsOnAll(EllipseFilter, AllInvalidShapes),

    RectangleFilter2 = shapes:shapesFilter2(rectangle),
    assertEquals(RectangleFilter2, [validShapes1()], {shapes, [validRectangle1(), validRectangle3()]}),
    assertEquals(RectangleFilter2, [validShapes2()], {shapes, [validRectangle1(), validRectangle3(), validRectangle4()]}),
    assertThrowsOnAll(RectangleFilter2, AllInvalidShapes),

    TriangleFilter2 = shapes:shapesFilter2(triangle),
    assertEquals(TriangleFilter2, [validShapes1()], {shapes, [validTriangle2()]}),
    assertEquals(TriangleFilter2, [validShapes2()], {shapes, [validTriangle2(), validTriangle3()]}),
    assertThrowsOnAll(TriangleFilter2, AllInvalidShapes),

    EllipseFilter2 = shapes:shapesFilter2(ellipse),
    assertEquals(EllipseFilter2, [validShapes1()], {shapes, [validEllipse1(), validEllipse1()]}),
    assertEquals(EllipseFilter2, [validShapes2()], {shapes, [validEllipse2()]}),
    assertThrowsOnAll(EllipseFilter2, AllInvalidShapes),

    SquareFilter2 = shapes:shapesFilter2(square),
    assertEquals(SquareFilter2, [validShapes1()], {shapes, [validRectangle3()]}),
    assertEquals(SquareFilter2, [validShapes2()], {shapes, [validRectangle3(), validRectangle4()]}),
    assertThrowsOnAll(SquareFilter2, AllInvalidShapes),

    CircleFilter2 = shapes:shapesFilter2(circle),
    assertEquals(CircleFilter2, [validShapes1()], {shapes, []}),
    assertEquals(CircleFilter2, [validShapes2()], {shapes, [validEllipse2()]}),
    assertThrowsOnAll(CircleFilter2, AllInvalidShapes).

