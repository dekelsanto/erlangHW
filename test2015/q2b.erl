-module(q2b).
-export([loop/1]).
-record(state, {variables=orddict:new()}).

get_input_and_parse() -> ok.
send_to_output(_Output) -> ok.

loop(State) ->
    case get_input_and_parse() of
        {assign, VariableName, Value} -> 
            NewState = assign(State, VariableName, Value),
            loop(NewState);

        {forget, VariableName} ->
            NewState = forget(State, VariableName),
            loop(NewState);

        {get, VariableName} ->
            send_to_output(orddict:fetch(VariableName, State#state.variables)),
            loop(State);

        {execute, Module, Func, Args} -> 
            send_to_output(erlang:apply(Module, Func, Args)),
            loop(State);

        {execute, VariableName, Module, Func, Args} ->
            NewState = assign(State, VariableName, erlang:apply(Module, Func, Args)),
            loop(NewState)
    end.
        




assign(State, VariableName, Value )->
    State#state{variables=orddict:store(VariableName, Value, State#state.variables)}.

forget(State, VariableName) ->
    State#state{variables=orddict:erase(VariableName, State#state.variables)}.
