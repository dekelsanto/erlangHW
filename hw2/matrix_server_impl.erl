-module(matrix_server_impl).
-export([init/0, loop/1, iterateRows/4]).
-include("matrix_server_records.hrl").

init() ->
    loop(#state{matrices=orddict:new()}).

loop(State) ->
    receive
        {Pid, MsgRef, {mult, Mat1, Mat2}} ->
            case State#state.active of
                false ->
                    NewState = State;
                true ->
                    NewState = handleNewMatrix(State, Pid, MsgRef, Mat1, Mat2)
            end,
            loop(NewState);

        {done, MsgRef, Row, Col, Value} ->
            UpdatedMatrixRecord = updateMatrix(State, MsgRef, Row, Col, Value),
            if
                UpdatedMatrixRecord#matrix.replies >= UpdatedMatrixRecord#matrix.elementCount ->
                    NewState = handleCompleteMatrix(State, MsgRef, UpdatedMatrixRecord);
                UpdatedMatrixRecord#matrix.replies < UpdatedMatrixRecord#matrix.elementCount ->
                    NewState = handleIncompleteMatrix(State, MsgRef, UpdatedMatrixRecord)
            end,
            loop(NewState);

        sw_upgrade ->
            % make:all([load]),
            ?MODULE:loop(State);

        {Pid, MsgRef, get_version} ->
            Pid ! {MsgRef, matrix_server:server_version()},
            loop(State);
            
        shutdown ->
            NewState = State#state{active = false},
            loop(NewState);

        _ ->
            loop(State)

        after 10 -> 
            case State#state.active == false andalso State#state.activeCount == 0 of
                true -> exit(normal);
                false -> continue
            end, 
            loop(State)
    end.

%%%% Aux functions

rowSize(Mat) -> tuple_size(matrix:getRow(Mat, 1)).
colSize(Mat) -> tuple_size(matrix:getCol(Mat, 1)).
getTargetMatrixSize(Mat1, Mat2) -> {colSize(Mat1), rowSize(Mat2)}.

iterateRows(Server, MsgRef, LhsMat, RhsMat) -> 
    iterateRows(Server, MsgRef, LhsMat, colSize(LhsMat), 1, RhsMat).
iterateRows(Server, MsgRef, LhsMat, LhsMatColSize, RowIndex, RhsMat) ->
    case RowIndex > LhsMatColSize of
        true -> ok;
        false ->
            spawnWorkersForLhsRow(Server, MsgRef, RowIndex, matrix_worker:tupleToList(matrix:getRow(LhsMat, RowIndex)), RhsMat),
            iterateRows(Server, MsgRef, LhsMat, LhsMatColSize, RowIndex + 1, RhsMat)
    end.

spawnWorkersForLhsRow(Server, MsgRef, RowIndex, Row, RhsMat) ->
    spawnWorkersForLhsRow(Server, MsgRef, RowIndex, Row, RhsMat, rowSize(RhsMat), 1).
spawnWorkersForLhsRow(Server, MsgRef, RowIndex, Row, RhsMat, RhsMatRowSize, ColIndex) ->
    case ColIndex > RhsMatRowSize of
        true -> ok;
        false ->
            spawn(matrix_worker, worker,
                [Server, MsgRef, RowIndex, ColIndex, Row, matrix_worker:tupleToList(matrix:getCol(RhsMat, ColIndex)), 0]
            ),
            spawnWorkersForLhsRow(Server, MsgRef, RowIndex, Row, RhsMat, RhsMatRowSize, ColIndex + 1)
    end.


%%%% Loop subroutines

handleNewMatrix(State, Pid, MsgRef, Mat1, Mat2) ->
    {RowSize, ColSize} = getTargetMatrixSize(Mat1, Mat2),
    MatrixRecord = #matrix{instance = matrix:getZeroMat(RowSize, ColSize), pid = Pid, elementCount = RowSize*ColSize},
    NewMatrices = orddict:store(MsgRef, MatrixRecord, State#state.matrices),
    spawn(?MODULE, iterateRows, [self(), MsgRef, Mat1, Mat2]),
    State#state{matrices=NewMatrices, activeCount=State#state.activeCount + 1}.

updateMatrix(State, MsgRef, Row, Col, Value) ->
    MatrixRecord = orddict:fetch(MsgRef, State#state.matrices),
    MatrixRecord#matrix{
        instance = matrix:setElementMat(Row, Col, MatrixRecord#matrix.instance, Value), 
        replies = MatrixRecord#matrix.replies + 1
    }.

handleCompleteMatrix(State, MsgRef, Matrix) ->
    Matrix#matrix.pid ! {MsgRef, Matrix#matrix.instance},
    NewMatrices = orddict:erase(MsgRef, State#state.matrices),
    State#state{matrices = NewMatrices, activeCount = State#state.activeCount - 1}.

handleIncompleteMatrix(State, MsgRef, Matrix) ->
    NewMatrices = orddict:store(MsgRef, Matrix, State#state.matrices),
    State#state{matrices=NewMatrices}.