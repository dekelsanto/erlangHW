-record(state, {
    active=true,
    activeCount=0,
    matrices
}).

-record(matrix, {
    replies=0,
    pid,
    elementCount,
    instance
}).
