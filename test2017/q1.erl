-module(q1).
-export([add/1, sortByComplexMetric/1, calc/1]).

add(L) -> add(L, {complex, 0, 0}).
add([], Sum) -> Sum;
add([H|T], Sum) -> 
    add(T, addComplex(H, Sum)).

mult(L) -> mult(L, {complex, 1, 0}).
mult([], Result) -> Result;
mult([H|T], Result) -> 
    mult(T, multComplex(H, Result)).


addComplex(N, Rhs={complex, _Rrhs, _Irhs}) when is_integer(N) ->
    addComplex({complex, N, 0}, Rhs);
addComplex({complex, Rlhs, Ilhs}, {complex, Rrhs, Irhs}) ->
    {complex, Rlhs+Rrhs, Irhs+Ilhs}.

multComplex(N, Rhs={complex, _Rrhs, _Irhs}) when is_integer(N) ->
    multComplex({complex, N, 0}, Rhs);
multComplex({complex, Rlhs, Ilhs}, {complex, Rrhs, Irhs}) -> 
    {complex, Rlhs*Rrhs - Ilhs*Irhs, Rlhs*Irhs + Ilhs*Rrhs}.


distanceFromZero(N) when is_integer(N) -> N;
distanceFromZero({complex, R, I}) ->
    math:sqrt(R*R + I*I).

complexMetric(N1, N2) -> 
    distanceFromZero(N1) - distanceFromZero(N2).

sortByComplexMetric([]) -> [];
sortByComplexMetric(L=[H|_T]) -> 
    sortByComplexMetric([X || X <- L, complexMetric(X, H) < 0]) ++
    [H] ++
    sortByComplexMetric([X || X <- L, complexMetric(X, H) > 0]).


flatten([]) -> [];
flatten([{Op, L} | T]) -> [calc({Op, L}) | flatten(T)];
flatten([H | T]) -> [H | flatten(T)].

calc({add, L}) ->  add(flatten(L));
calc({mult, L}) -> mult(flatten(L)).