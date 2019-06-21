-module(shapes).
-export([shapesArea/1, squaresArea/1, trianglesArea/1, shapesFilter/1, shapesFilter2/1]).

% DEKEL SANTO 203024443 SPRING 2019

%%%% Validation functions
isPolygonValid(MeasurementType, M1, M2) 
    when MeasurementType =:= dim, M1 > 0, M2 > 0 ->
        true.

isEllipseValid(MeasurementType, R1, R2)
    when MeasurementType =:= radius, R1 > 0, R2 > 0 ->
        true.

isShapeValid({ShapeType, {MeasurementType, M1, M2}}) ->
    if 
        ShapeType =:= rectangle -> isPolygonValid(MeasurementType, M1, M2);
        ShapeType =:= triangle -> isPolygonValid(MeasurementType, M1, M2);
        ShapeType =:= ellipse -> isEllipseValid(MeasurementType, M1, M2)
    end.

isSquare({rectangle, {dim, M, M}}) -> true;
isSquare(_Shape) -> false.

isCircle({ellipse, {radius, M, M}}) -> true;
isCircle(_Shape) -> false.



%%%% Auxiliary functions
getArea({ShapeType, {MeasurementType, M1, M2}}) ->
    Mult=M1*M2,
    if 
        ShapeType =:= rectangle -> isPolygonValid(MeasurementType, M1, M2), Mult;
        ShapeType =:= triangle -> isPolygonValid(MeasurementType, M1, M2), Mult/2;
        ShapeType =:= ellipse -> isEllipseValid(MeasurementType, M1, M2), Mult*math:pi()
    end.




%%%% shapesArea/1
shapesArea({shapes, ShapeArray}) -> 
    shapesArea(ShapeArray, 0).

shapesArea([], Sum) -> Sum;
shapesArea([ CurrentShape | NextShapes ], Sum) ->
    getArea(CurrentShape) + shapesArea(NextShapes, Sum). % eventually will return the sum of all shape areas




%%%% squaresArea/1
squaresArea({shapes, ShapeArray}) ->
    try squaresArea(ShapeArray, 0) of
        Val -> Val
    catch
        throw:false -> 0
    end.

squaresArea([], Sum) -> Sum;
squaresArea([ {rectangle, {dim, Edge, Edge}} | NextShapes ], Sum) when Edge > 0 -> % only squares get added to the sum
    Edge*Edge + squaresArea(NextShapes, Sum); 
squaresArea([ CurrentShape | NextShapes ], Sum) ->
    isShapeValid(CurrentShape),
    squaresArea(NextShapes, Sum).




%%%% trianglesArea/1
trianglesArea({shapes, ShapeArray}) ->
    try trianglesArea(ShapeArray, 0) of
        Val -> Val
    catch
        throw:false -> 0
    end.

trianglesArea([], Sum) -> Sum;
trianglesArea([{triangle, {dim, Base, Height}} | NextShapes ], Sum) when Base > 0, Height > 0 -> % very similar to squaresArea/1
    Base*Height/2 + trianglesArea(NextShapes, Sum);
trianglesArea([ CurrentShape | NextShapes ], Sum) ->
    isShapeValid(CurrentShape),
    trianglesArea(NextShapes, Sum).




%%%% shapesFilter/1
shapesFilter(ShapeType) -> 
     fun({shapes, ShapeArray}) ->
        { shapes, [Shape || Shape <- ShapeArray, isShapeValid(Shape), element(1, Shape) =:= ShapeType ] } end. % generate a list where each shape's first element is the requested shape type. Would look better in python :)




%%%% shapesFilter2/1
shapesFilter2(ShapeType) when ShapeType =:= square -> % exclusive filter for squares using validation function
    fun({shapes, ShapeArray}) -> 
        { shapes, [ Shape || Shape <- ShapeArray, isShapeValid(Shape), isSquare(Shape)] } end; 
shapesFilter2(ShapeType) when ShapeType =:= circle -> % ^^
    fun({shapes, ShapeArray}) -> 
        { shapes, [ Shape || Shape <- ShapeArray, isShapeValid(Shape), isCircle(Shape)] } end; 
shapesFilter2(ShapeType) -> % otherwise use previous implementation
    shapesFilter(ShapeType). 


