-module(q1b).

-export([solver/2]).

solver(_Num, []) -> none;
solver(Num, [Func | MoreFuncs]) -> 
    case Func(Num) of
        0 -> Func;
        _ -> solver(Num, MoreFuncs)
    end.