-module(q1b).

-export([isAlgebric/1, isGeometric/1, isSeries/2, genSeries/4, genSeries/3]).

isAlgebric(L) -> listSatisfiesCondition(fun(X,Y)->Y-X end, L).
isGeometric(L) -> listSatisfiesCondition(fun(X,Y)->Y/X end, L).

listSatisfiesCondition(_Cond, [SingleItem]) when is_integer(SingleItem) -> true;
listSatisfiesCondition(Cond, [Item1, Item2 | T]) -> listSatisfiesCondition(Cond, [Item2 | T], catch( Cond(Item1, Item2) )).

listSatisfiesCondition(_Cond, [_SingleItem], _Diff) -> true;
listSatisfiesCondition(Cond, [Item1, Item2 | T], Diff) -> 
    try Cond(Item1, Item2) of 
        Diff -> listSatisfiesCondition(Cond, [Item2 | T], Diff);
        _ -> false
    catch
        error:_ ->
            false
    end.


isSeries([_SingleItem], _F) -> true;
isSeries([Item1, Item2 | T], F) -> 
    case F(Item1) of
        Item2 -> isSeries([Item2 | T], F);
        _ -> false
    end.

genSeries(special, A0, GeneratorFunc, ElemCount) -> 
    genSeriesImpl(GeneratorFunc, ElemCount-1, [A0]);
genSeries(algebraic, A0, D, ElemCount) ->
    genSeriesImpl(fun(X)->X+D end, ElemCount-1, [A0]);
genSeries(geometric, A0, Q, ElemCount) ->
    genSeriesImpl(fun(X)->X*Q end, ElemCount-1, [A0]).

genSeriesImpl(_GeneratorFunc, 0, L) -> lists:reverse(L);
genSeriesImpl(GeneratorFunc, ElemCount, L=[An | _T]) ->
    Anp1 = GeneratorFunc(An),
    genSeriesImpl(GeneratorFunc, ElemCount-1, [Anp1 | L]).

genSeries(special, A0, GeneratorFunc) -> 
    [A0 | fun()->genSeries(special, GeneratorFunc(A0), GeneratorFunc) end ];
genSeries(algebraic, A0, D) ->
    genSeries(special, A0, fun(X)->X+D end);
genSeries(geometric, A0, Q) ->
    genSeries(special, A0, fun(X)->X*Q end).