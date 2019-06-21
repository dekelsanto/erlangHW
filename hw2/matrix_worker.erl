-module(matrix_worker).
-export([worker/7, tupleToList/1]).

tupleToList(Tuple) ->
    [element(I, Tuple) || I <- lists:seq(1, tuple_size(Tuple))].

worker(Server, MsgRef, RowIndex, ColIndex, [], [], Sum) ->
    Server ! {done, MsgRef, RowIndex, ColIndex, Sum};
worker(Server, MsgRef, RowIndex, ColIndex, [RowValue | RowNext], [ColValue | ColNext], Sum) ->
    worker(Server, MsgRef, RowIndex, ColIndex, RowNext, ColNext, Sum + RowValue*ColValue).