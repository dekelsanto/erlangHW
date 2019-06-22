-module(loadBalance).
-behaviour(supervisor).

-export([init/1]).
-export([startServers/0, stopServers/0]).
-export([numberOfRunningFunctions/1, calcFun/3]).

init([]) -> 
    Server1 = {server1, {serverInstance, start_link, [server1]},
        permanent, 2000, worker, []},
    Server2 = {server2, {serverInstance, start_link, [server2]},
        permanent, 2000, worker, []},
    Server3 = {server3, {serverInstance, start_link, [server3]},
        permanent, 2000, worker, []},
    {ok, {{one_for_one, 10, 1}, [Server1, Server2, Server3]}}.

startServers() ->
    make:all([load]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stopServers() ->
    exit(whereis(?MODULE), normal).

numberOfRunningFunctions(Id) ->
    ServerName = case Id of 
        1 -> server1;
        2 -> server2;
        3 -> server3
    end, 
    gen_server:call(ServerName, {count}).

calcFun(Pid, Fun, MsgRef) ->
    TargetServer = selectServer([server1, server2, server3]),
    gen_server:cast(TargetServer, {runFunc, {Pid, Fun, MsgRef}}).


selectServer([DefaultServer | MoreServers]) -> 
    selectServer(MoreServers, DefaultServer, gen_server:call(DefaultServer, {count})).

selectServer([], CurrentServer, _CurrentFuncCount) -> CurrentServer;
selectServer(_MoreServers, CurrentServer, 0) -> CurrentServer;
selectServer([CandidateServer | MoreServers], CurrentServer, CurrentFuncCount) ->
    CandidateFuncCount = gen_server:call(CandidateServer, {count}),
    {WinnerServer, WinnerFuncCount} = case CandidateFuncCount < CurrentFuncCount of
        true -> {CandidateServer, CandidateFuncCount};
        false -> {CurrentServer, CurrentFuncCount}
    end, 
    selectServer(MoreServers, WinnerServer, WinnerFuncCount).