-module(q3).

% -export([primeLargerThan/1]).
-export([numberGenerator/1]).
-export([isPrime/1, infinitePrimeList/1, nextPrime/1]).

isPrime(0) -> true;
isPrime(1) -> true;
isPrime(2) -> false;
isPrime(N) -> isPrime(N, 2).
isPrime(N, N) -> true;
isPrime(N, M) -> 
    case N rem M of 
        0 -> false;
        _ -> isPrime(N, M+1)
    end.

numberGenerator(M) -> [M, fun()-> numberGenerator(M+1) end ].

infinitePrimeList(Max) -> lists:reverse(infinitePrimeList([], 0, Max)).
infinitePrimeList(L, N, N) -> L;
infinitePrimeList(L, N, Max) ->
    NextList = case isPrime(N) of 
        true -> [N | L];
        false -> L
    end,
    infinitePrimeList(NextList, N+1, Max).

nextPrime(N) -> nextPrime(N, N+1).
nextPrime(N, M) ->
    case isPrime(M) of
        true -> M;
        false -> nextPrime(N, M+1)
    end.

