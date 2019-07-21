-module(q1a).
-behaviour(gen_server).

%% API
-export([start_link/0, serverSolve/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([solver/2]).
-record(state, {dummy}).

start_link() ->
   gen_server:start_link({local, solver_server}, ?MODULE, [], []).

init(_Args) ->
   {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({solve, {Fun, List}}, _From, State) ->
    {reply, solver(Fun, List), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.



solver(_Fun, []) -> none;
solver(Fun, [CurrentValue | MoreValues]) ->
    case Fun(CurrentValue) == 0 of
        true -> CurrentValue;
        false -> solver(Fun, MoreValues)
    end.


serverSolve(Fun, List) -> 
    gen_server:call(solver_server, {solve, {Fun, List}}).