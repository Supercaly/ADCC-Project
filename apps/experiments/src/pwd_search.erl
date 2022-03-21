-module(pwd_search).

-export([
    master_task/0,
    worker_task/0
]).

master_task() ->
    % insert mock data to pwd_space
    Hashes = lists:map(fun(PwdInt) ->
        Pwd = integer_to_list(PwdInt),
        Hash = create_hash(Pwd),
        ts:out(pwd_space, {Pwd, Hash}),
        Hash
    end, lists:seq(0,10)),        

    % send hash requests to workers
    lists:foreach(fun(Hash) ->
        ts:out(task_space, {search_task, Hash})
    end, Hashes),

    % wait for worker to respond with passwords
    wait_for_pwd(10),
    ok.

worker_task() ->
    % get a password task
    {ok, {search_task, Hash}} = ts:in(task_space,{search_task, any}),
    % find the pwd for given hash in the pwd space
    {ok, {Pwd, _Hash}} = ts:rd(pwd_space, {any,Hash}),
    % write found pwd in space
    ts:out(task_space, {found_password, Hash, Pwd}),
    ok.

create_hash(Data) ->
    io_lib:format("~64.16.0b", 
        [binary:decode_unsigned(crypto:hash(sha256, Data))]).

wait_for_pwd(0) -> ok;
wait_for_pwd(N) ->
    {ok, {found_password, Hash, Pwd}} = ts:in(task_space, {found_password, any, any}),
    global:whereis_name(sup)!{got_pwd, Hash, Pwd},
    wait_for_pwd(N-1).