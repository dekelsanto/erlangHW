-module(matrix_server_sup).
-export([restarter/0]).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(matrix_server_impl, init, []),
    register(matrix_server, Pid),
receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter()
end.