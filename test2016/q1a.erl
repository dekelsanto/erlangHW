-module(q1a).
-export([checker/4]).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {hwtests}).

start_link() ->
   gen_server:start_link({local, checkerServer}, ?MODULE, [], []).

init(_Args) ->
   {ok, #state{hwtests=orddict:new()}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({check_hw, Num, F}, _From, State) ->
    case orddict:is_key(Num, State#state.hwtests) of 
        true ->
            {L1, L2, L3} = orddict:fetch(Num, State#state.hwtests),
            Result = checker(F, L1, L2, L3),
            {reply, Result, State};
        false ->
            {reply, error, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_hw_testing, {Num, L1, L2, L3}}, State) ->
    NewState = State#state{hwtests = orddict:store(Num, {L1, L2, L3}, State#state.hwtests)},
    {noreply, NewState};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.






checker(Func, L1, L2, L3)->
    checker(Func, L1, L2, L3, 0).

checker(_Func, [], [], [], Sum) -> Sum;
checker(Func, [H1 | T1], [H2 | T2], [H3 | T3], Sum)->
    try Func(H1) of 
        Result -> 
            NewSum = case Result == H2 of 
                true -> Sum + H3;
                false -> Sum
            end,
            checker(Func, T1, T2, T3, NewSum)
    catch
        error:_ -> 
            checker(Func, T1, T2, T3, Sum)
    end.