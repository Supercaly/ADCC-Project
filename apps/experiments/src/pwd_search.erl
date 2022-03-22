-module(pwd_search).

-export([
    init_spaces/0,
    populate_pwd/1,
    master_task/1,
    worker_task/0
]).

init_spaces() ->
    ts:new(pwd_space),
    ts:new(task_space),
    lists:foreach(fun(Node) ->
        ts:addNode(pwd_space, Node),
        ts:addNode(task_space, Node)
    end, nodes()).

% external task populating the database with hashed passwords
populate_pwd(N) ->
    lists:map(fun(PwdInt) -> 
        Pwd = integer_to_list(PwdInt),
        Hash = create_hash(Pwd),
        ts:out(pwd_space, {Pwd, Hash}),
        Hash
    end, lists:seq(0,N-1)).

master_task(Hashes) ->      
    % sends hash requests to workers
    lists:foreach(fun(Hash) ->
        ts:out(task_space, {search_task, Hash}),
        io:format("cerca:~p~n",[Hash])
    end, Hashes),

    io:format("sono qui~n"),

    % wait for worker to respond with passwords
    wait_for_pwd(10),
    ok.

worker_task() ->
    % get a password task
    {ok, {search_task, Hash}} = ts:in(task_space,{search_task, any}),
    io:format("devo cercare:~p~n",[Hash]),
    % find the pwd for given hash in the pwd space
    {ok, {Pwd, _Hash}} = ts:rd(pwd_space, {any,Hash}),
    % write found pwd in space
    ts:out(task_space, {found_password, Hash, Pwd}),
    worker_task(),
    ok.

create_hash(Data) ->
    io_lib:format("~64.16.0b", 
        [binary:decode_unsigned(crypto:hash(sha256, Data))]).

wait_for_pwd(0) -> ok;
wait_for_pwd(N) ->
    {ok, {found_password, _Hash, _Pwd}} = ts:in(task_space, {found_password, any, any}),
    io:format("trovata:~p~n",[_Pwd]),
    wait_for_pwd(N-1).