-module(q2b).
-behaviour(gen_server).

%% API
-export([start_link/0, calcOnServer/2, getBalance/1, payBalance/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).
-record(users, {username, debt}).


start_link() ->
    mnesia:create_schema(nodes()),
    mnesia:start(),
    mnesia:create_table(users, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, users)}]),
    gen_server:start_link({local, calcServer}, ?MODULE, [], []).

init(_Args) ->
   {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({exec, User, F}, _From, State) ->
    Result = worker(User, F),
    {reply, Result, State};

handle_call({balance, User}, _From, State)->
    {atomic, [UserRecord]} = mnesia:transaction(fun() -> mnesia:read({users, User}) end ),
    {reply, UserRecord#users.debt, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({pay, User}, State)->
    {atomic, [UserRecord]} = mnesia:transaction(fun() -> mnesia:read({users, User}) end ),
    UpdatedUserRecord = UserRecord#users{debt = 0},
    mnesia:transaction(fun() -> mnesia:write(UpdatedUserRecord) end ),
    {noreply, State};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

worker(User, F) ->
    StartTime = erlang:timestamp(),
    Result = try F() of
        R -> R
    catch 
        error:_ -> error
    end, 
    EndTime = erlang:timestamp(),
    TimeSpent = timer:now_diff(EndTime, StartTime) / 1000000,
    UpdatedUserRecord = case mnesia:transaction(fun() -> mnesia:read({users, User}) end ) of 
        {atomic, [UserRecord]} -> 
            UserRecord#users{debt = UserRecord#users.debt+TimeSpent};
        {atomic, []} ->
            #users{username = User, debt = TimeSpent}
    end,
    mnesia:transaction(fun() -> mnesia:write(UpdatedUserRecord) end ),
    Result.



calcOnServer(User, F) ->
    gen_server:call(calcServer, {exec, User, F}).

getBalance(User)->
    gen_server:call(calcServer, {balance, User}).

payBalance(User) ->
    gen_server:cast(calcServer, {pay, User}).


