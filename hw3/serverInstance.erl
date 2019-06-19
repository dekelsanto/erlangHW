-module(serverInstance).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {name, functionCount}).

stop(Name) ->
   gen_server:call(Name, stop).

start_link(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init(Name) ->
    io:format("~p starting~n", Name),
   {ok, #state{name=Name, functionCount=0}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({count}, _From, State) ->
    {reply, State#state.functionCount, State};

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


% worker(Pid, )




