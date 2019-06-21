-module(matrix_server).
-export([start_server/0, shutdown/0, mult/2, get_version/0, explanation/0, server_version/0]).


start_server() ->
    make:all([load]),
    spawn(matrix_server_sup, restarter, []).


shutdown() ->
    matrix_server ! shutdown.

mult(Mat1, Mat2) ->
    rpc({mult, Mat1, Mat2}).

get_version() -> rpc(get_version).

explanation() ->
    "The supervisor and the server implementation are separated for two reasons: " ++
    "A) Code re-use: Supervisor code is usually generic and can be re-used for other servers; " ++
    "B) Dynamic server code: Separate server module allows for code changes during runtime.".

server_version() -> version_1.

rpc(Data) -> 
    Ref = make_ref(),
    matrix_server ! {self(), Ref, Data},
    receive
        {Ref, Response} -> Response
    end.

