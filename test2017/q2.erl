-module(q2).
-behaviour(gen_server).

%% API
-export([startServer/0, stopServer/0, calcOnServer/1, calcOnServer/2]).

-export([start_link/0, worker/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

start_link() ->
%    gen_server:start_link({local, funcServer}, ?MODULE, [], []).
    gen_server:start_link({local, funcServer}, ?MODULE, [], []),
    gen_server:start_link({local, funcServer2}, ?MODULE, [], []),
    gen_server:start_link({local, funcServer3}, ?MODULE, [], []),
    gen_server:start_link({local, funcServer4}, ?MODULE, [], []),
    gen_server:start_link({local, funcServer5}, ?MODULE, [], []).

init(_Args) ->
   {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    gen_server:stop(funcServer),
    gen_server:stop(funcServer2),
    gen_server:stop(funcServer3),
    gen_server:stop(funcServer4),
    gen_server:stop(funcServer5),
   {stop, normal, stopped, State};

handle_call({exec, F}, _From, State)->
    {reply, F(), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({exec, F, Pid, MsgRef}, State) ->
    spawn(?MODULE, worker, [F, Pid, MsgRef]),
    {noreply, State};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

worker(F, Pid, MsgRef) -> 
    Pid ! {F(), MsgRef}.


startServer() -> start_link().
stopServer() -> gen_server:call(funcServer, stop).

calcOnServer(F) -> 
    MsgRef = make_ref(),
    gen_server:cast(funcServer, {exec, F, self(), MsgRef}),
    receive
        {Result, MsgRef} -> Result
    end.
    % gen_server:call(funcServer, {exec, F}).

calcOnServer(F, Num) when is_integer(Num) -> 
    Server = case Num of 
        1 -> funcServer;
        2 -> funcServer2;
        3 -> funcServer3;
        4 -> funcServer4;
        5 -> funcServer5
    end, 
    MsgRef = make_ref(),
    gen_server:cast(Server, {exec, F, self(), MsgRef}),
    receive
        {Result, MsgRef} -> Result
    end.

