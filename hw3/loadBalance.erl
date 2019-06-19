-module(loadBalance).
-behaviour(supervisor).

-export([init/1, startServers/0, stopServers/0]).
init([]) -> 
    Server1 = {server1, {serverInstance, start_link, [server1]},
        permanent, 2000, worker, []},
    Server2 = {server2, {serverInstance, start_link, [server2]},
        permanent, 2000, worker, []},
    Server3 = {server3, {serverInstance, start_link, [server3]},
        permanent, 2000, worker, []},
    {ok, {{rest_for_one, 1, 1}, [Server1, Server2, Server3]}}.

startServers() ->
    make:all([load]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stopServers() ->
    exit(whereis(?MODULE), normal).
