-module(q3b).
-export([derivateArray/2]).

derivate(F, X) -> (F(X+1) - F(X-1)) / 2.

% derivateArray(F, L) -> 
%     DerivativeAtPoint = fun(X) -> derivate(F, X) end,
%     lists:map(DerivativeAtPoint, L).

derivateArray(F, L) -> 
    derivateArray(F, L, []).

derivateArray(_F, [], L) -> lists:reverse(L);
derivateArray(F, [H|T], L) ->
    derivateArray(F, T, [derivate(F, H) | L]).