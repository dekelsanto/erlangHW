-module(q2a).

-export([solver/3, map2d/3]).

solver(_Func, CurrentValue, Upper) when CurrentValue >= Upper -> none;
solver(Func, CurrentValue, Upper) -> 
    case abs(Func(CurrentValue)) =< 0.1 of 
        true -> CurrentValue;
        false -> solver(Func, CurrentValue + 0.1, Upper)
    end.


map2d(Func, L1, L2) -> map2d(Func, L1, L2, []).

map2d(_Func, [], _L2, Result) -> Result;
map2d(Func, [H | T], L2, Result) ->
    map2d(Func, T, L2, Result ++ lists:map(fun(X)->Func({H, X}) end, L2)).
