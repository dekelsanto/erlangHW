-module(game).
-export([canWin/1, nextMove/1, explanation/0]).

% DEKEL SANTO 203024443 SPRING 2019

canWin(N) when is_integer(N) -> canWinGuarded(N).

canWinGuarded(N) when N == 0 -> false;
canWinGuarded(N) when N > 0, N =< 2 -> true;
canWinGuarded(N) when N > 2 -> not canWinGuarded(N-1) or not canWinGuarded(N-2). % similar to fib(N) from the lecture


nextMove(N) when N == 0 -> false;
nextMove(N) when is_integer(N), N > 0 ->
    case canWin(N-1) of % attempt to push the other player into a situation where he cannot win
        false -> {true, 1}; % this will lead the opponent to a dead end
        true -> % otherwise try second option
            case canWin(N-2) of
                false -> {true, 2}; % again opponent dead end
                true -> false % opponent can win and no options left :(
            end
    end.
                
explanation() ->
    { "Tail recursion can only generate a single trail to the base case where the recursion stops. " ++
        "The functions canWin/1 and nextMove/1 may need to check multiple recursion trails- for example, " ++
        "if the player cannot win by picking a single match, the option of picking two matches must also be checked" }.