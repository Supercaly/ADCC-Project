-module(matrices).

-export([
    matrix_multiplication_task/2
]).

% Perform the matrix multiplication test case
% NOTE: This code is run by the supervisor node
matrix_multiplication_task(NWorkers, MatrixSize) ->
    nodelib:init_sup(),
    {[Master], Workers} = nodelib:spawn_nodes(1, NWorkers),
    lists:foreach(fun(N) ->
        ok = rpc:call(N, proflib, start, ["./profiler/matrix/"])
    end, [Master]++Workers),

    init_space(),
    
    nodelib:run_on_node(Master, fun() ->
        proflib:begine(task),
        master_task(MatrixSize),
        proflib:ende(task)
    end),

    lists:foreach(fun(Node) ->
        nodelib:run_on_node(Node, fun() ->
            proflib:begine(task),
            worker_task(trunc(math:sqrt(MatrixSize))),
            proflib:ende(task)
        end)
    end, Workers),

    nodelib:wait_for_master(),
    ok.

% Initialize the needed space
init_space() ->
    ok = ts:new(matrix_space),
    lists:foreach(fun(Node) ->
        ok = ts:addNode(matrix_space, Node)
    end, nodes()).

% Create a squared matrix of given size 
% populated with random integer values
generate_matrix(Size) -> 
    [rand:uniform(10) || _ <- lists:seq(1,Size)].

% Split a squared matrix by rows and return a list
% of each rows
split_matrix_by_row(Matrix, Size) ->
    RowLen = trunc(math:sqrt(Size)),
    [lists:sublist(Matrix, X, RowLen) || X <- lists:seq(1,Size,RowLen)].

% Task run by the master node
% NOTE: This code is run by the master node
master_task(MatrixSize) -> 
    % generate two matrices
    MatrixA = generate_matrix(MatrixSize),
    MatrixB = generate_matrix(MatrixSize),
    io:format("Matrix A:~n"),
    print_matrix(MatrixA, MatrixSize),
    io:format("Matrix B:~n"),
    print_matrix(MatrixB, MatrixSize),
    
    % send each element of matrix A as a message
    lists:foldl(fun(Elem, Index) ->
        proflib:begine(write_matrix_a),
        ts:out(matrix_space, {matrixA, Index, Elem}),
        proflib:ende(write_matrix_a),
        Index + 1
    end, 0, MatrixA),

    % send each line of matrix B as a message
    % Note: each line must be sent n-rows times since it's required
    % more than once
    MatrixBRows = split_matrix_by_row(MatrixB, MatrixSize),
    lists:foreach(fun(Rep) ->
        lists:foldl(fun(Row, Index) ->
            proflib:begine(write_matrix_b),
            ts:out(matrix_space, {matrixB, Index, Row, Rep}),
            proflib:ende(write_matrix_b),
            Index+1
        end, 0, MatrixBRows)
    end, lists:seq(1, trunc(math:sqrt(MatrixSize)))),
    
    io:format("Node '~p' has sent all his requests~n", [node()]),
    
    % wait for the workers to respond with matrix C
    Res = wait_for_matrix([], MatrixSize),
    io:format("Matrix C:~n"),
    print_matrix(Res, MatrixSize),

    % Tell the supervisor the task is complete
    case nodelib:get_supervisor() of
        undefined -> exit({supervisor_undefined});
        Pid -> Pid!{finished, node()}
    end,
    ok.

% Task run by the worker node
% NOTE: This code is run by the worker node
worker_task(RowSize) ->
    % wait for one element of matrix A
    proflib:begine(read_matrix_a),
    {ok, {_, Idx, ValA}} = ts:in(matrix_space, {matrixA, any, any}),
    proflib:ende(read_matrix_a),
   
    % wait for the corresponding row of matrix B
    proflib:begine(read_matrix_b),
    {ok, {_, _, RowB, _}} = ts:in(matrix_space, {matrixB, Idx rem RowSize, any, any}),
    proflib:ende(read_matrix_b),

    % perform partial multiplication
    RowC = lists:map(fun(E) -> E * ValA end, RowB),

    % send the calculated row of matrix C
    proflib:begine(write_matrix_c),
    ts:out(matrix_space, {matrixC, Idx div RowSize, RowC, rand:uniform()}),
    proflib:ende(write_matrix_c),

    worker_task(RowSize),
    ok.

% Wait for all the workers to output thei partial calculation
% This function takes as parameter the partial matrix and his size
% and returns a matrix list.
% Note: Internally the matrix is represented as a list of rows
wait_for_matrix([], N) -> 
    Len = trunc(math:sqrt(N)),
    EmptyMatrix = [[0 || _ <- lists:seq(1,Len)] || _ <- lists:seq(1,Len)],
    wait_for_matrix(EmptyMatrix, N);
wait_for_matrix(Result, 0) -> lists:append(Result);
wait_for_matrix(PartialMatrix, N) ->
    % wait for a computed row of matrix C
    proflib:ende(read_matrix_c),
    {ok, {_, Idx, ValC, _}} = ts:in(matrix_space, {matrixC, any, any, any}),
    proflib:ende(read_matrix_c),
    
    % append the row to the matrix
    {NewMatrix,_} = lists:mapfoldl(fun(Row, Index) ->
        if 
            Idx == Index -> {lists:zipwith(fun(E1,E2) -> E1+E2 end, ValC, Row), Index+1};
            true -> {Row, Index+1}
        end
    end, 0, PartialMatrix),
        
    % wait for another row
    wait_for_matrix(NewMatrix, N-1).

% Print a matrix to standrard output
print_matrix(Matrix, Size) ->
    lists:foreach(fun(R) -> 
        io:format("  "),
        lists:foreach(fun(E) ->
            io:format("~p ",[E])
        end, R),
        io:format("~n")
    end, split_matrix_by_row(Matrix, Size)).