-module(pwd_search).

-export([
    pwd_search_task/3
]).

% Perform the password search test case
% NOTE: This code is run by the supervisor node
%
% Parameters:
%  NNodes number of worker nodes
%  WorkersPerNode number of workers per node
%  NPwds number of passwords to generate
pwd_search_task(NNodes, WorkersPerNode, NPwds) ->
    nodelib:init_sup(),
    {[Master], Workers} = nodelib:spawn_nodes(1, NNodes),
    lists:foreach(fun(N) ->
        ok = rpc:call(N, proflib, start, ["./profiler/pwd/"])
    end, [Master]++Workers),

    init_spaces(),
    Hashes = populate_pwd(NPwds),

    nodelib:run_on_node(Master, fun() ->
        master_task(Hashes),
        % tell the supervisor the work is done
        case nodelib:get_supervisor() of
            undefined -> exit({supervisor_undefined});
            Pid -> Pid!{finished, node()}
        end
    end),

    lists:foreach(fun(Node) ->
        lists:foreach(fun(TaskID) ->
            nodelib:run_on_node(Node, fun() -> worker_task(TaskID) end)
        end, lists:seq(0, WorkersPerNode-1))
    end, Workers),

    nodelib:wait_for_master(),
    ok.

% Initialize the needed spaces
init_spaces() ->
    ok = ts:new(pwd_space),
    ok = ts:new(task_space),
    lists:foreach(fun(Node) ->
        ok = ts:addNode(pwd_space, Node),
        ok = ts:addNode(task_space, Node)
    end, nodes()).

% Populate the database with N passwords and hashes
populate_pwd(N) ->
    lists:map(fun(PwdInt) -> 
        Pwd = integer_to_list(PwdInt),
        Hash = create_hash(Pwd),
        ok = ts:out(pwd_space, {Pwd, Hash}),
        Hash
    end, lists:seq(0,N-1)).

% Task run by the master node
% NOTE: This code is run by the master node
master_task(Hashes) -> 
    proflib:begin_event(task),
    
    % sends hash requests to workers
    lists:foreach(fun(Hash) ->
        proflib:begin_event(ask_for_hash),
        ok = ts:out(task_space, {search_task, Hash}),
        proflib:end_event(ask_for_hash)
    end, Hashes),

    io:format("Node '~p' has sent all his requests~n", [node()]),

    % wait for worker to respond with passwords
    ok = wait_for_passwords(length(Hashes)),
    
    proflib:end_event(task),
    ok.

% Task run by the worker node
% NOTE: This code is run by the worker node
worker_task(TaskID) ->
    % wait for new hash to search
    proflib:begin_task(TaskID, read_task),
    {ok, {search_task, Hash}} = ts:in(task_space,{search_task, any}),
    proflib:end_task(TaskID, read_task),
    
    % find the pwd for given hash in the pwd_space
    proflib:begin_task(TaskID, search_pwd),
    {ok, {Pwd, _Hash}} = ts:rd(pwd_space, {any,Hash}),
    proflib:end_task(TaskID, search_pwd),
    
    % respond with the found password
    proflib:begin_task(TaskID, answer_pwd),
    ok = ts:out(task_space, {found_password, Hash, Pwd}),
    proflib:end_task(TaskID, answer_pwd),
    
    % search next hash
    worker_task(TaskID),
    ok.

% Return a string hash from some data
create_hash(Data) ->
    io_lib:format("~64.16.0b", 
        [binary:decode_unsigned(crypto:hash(sha256, Data))]).

% Wait for workers to find N passwords then finish the task
wait_for_passwords(0) -> ok;
wait_for_passwords(N) ->
    proflib:begin_event(read_result),
    {ok, {found_password, _Hash, _Pwd}} = ts:in(task_space, {found_password, any, any}),
    proflib:end_event(read_result),
    wait_for_passwords(N-1).