-module(pwd_search).

-export([
    pwd_search_task/2
]).

% Perform the password search test case
% NOTE: This code is run by the supervisor node
pwd_search_task(NWorkers, NPwds) ->
    nodelib:init_sup(),
    {[Master], Workers} = nodelib:spawn_nodes(1, NWorkers),
    lists:foreach(fun(N) ->
        ok = rpc:call(N, proflib, start, ["./profiler/pwd/"])
    end, [Master]++Workers),

    init_spaces(),
    Hashes = populate_pwd(NPwds),

    nodelib:run_on_node(Master, fun() ->
        proflib:begine(task),
        master_task(Hashes),
        proflib:ende(task),
        case nodelib:get_supervisor() of
            undefined -> exit({supervisor_undefined});
            Pid -> Pid!{finished, node()}
        end
    end),

    lists:foreach(fun(Node) ->
        nodelib:run_on_node(Node, fun() -> worker_task() end)
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
    % sends hash requests to workers
    lists:foreach(fun(Hash) ->
        proflib:begine(ask_for_hash),
        ok = ts:out(task_space, {search_task, Hash}),
        proflib:ende(ask_for_hash)
    end, Hashes),

    io:format("Node '~p' has sent all his requests~n", [node()]),

    % wait for worker to respond with passwords
    ok = wait_for_passwords(length(Hashes)),
    ok.

% Task run by the worker node
% NOTE: This code is run by the worker node
worker_task() ->
    % wait for new hash to search
    proflib:begine(read_task),
    {ok, {search_task, Hash}} = ts:in(task_space,{search_task, any}),
    proflib:ende(read_task),
    
    % find the pwd for given hash in the pwd_space
    proflib:begine(search_pwd),
    {ok, {Pwd, _Hash}} = ts:rd(pwd_space, {any,Hash}),
    proflib:ende(search_pwd),
    
    % respond with the found password
    proflib:begine(ansker_pwd),
    ok = ts:out(task_space, {found_password, Hash, Pwd}),
    proflib:ende(ansker_pwd),
    
    % search next hash
    worker_task(),
    ok.

% Return a string hash from some data
create_hash(Data) ->
    io_lib:format("~64.16.0b", 
        [binary:decode_unsigned(crypto:hash(sha256, Data))]).

% Wait for workers to find N passwords then finish the task
wait_for_passwords(0) -> ok;
wait_for_passwords(N) ->
    proflib:begine(read_result),
    {ok, {found_password, _Hash, _Pwd}} = ts:in(task_space, {found_password, any, any}),
    proflib:ende(read_result),
    wait_for_passwords(N-1).