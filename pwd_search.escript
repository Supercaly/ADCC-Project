#! /usr/local/bin/escript
%%! -pa ebin -sname supervisor@localhost -secret cookie

-export([main/1]).

create_hash(Data) ->
    io_lib:format("~64.16.0b", 
        [binary:decode_unsigned(crypto:hash(sha256, Data))]).

main(_Arg) ->
    ts:start(),
    global:register_name(sup, self()),
    
    [Master|Workers] = nodelib:spawn_nodes(3),

    % create pwd space and working space
    ts:new(pwd_space),
    ts:new(task_space),
    lists:foreach(fun(Node) ->
        ts:addNode(pwd_space, Node),
        ts:addNode(task_space, Node)
    end, [Master]++Workers),
    
    nodelib:run_on_node(Master, fun() ->
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
        wait_for_pwd(10)
    end),
    lists:foreach(fun(Worker) -> nodelib:run_on_node(Worker, fun() ->
        {ok, {search_task, Hash}} = ts:in(task_space,{search_task, any}),
        {ok, {Pwd, _Hash}} = ts:rd(pwd_space, {any,Hash}),
        ts:out(task_space, {found_password, Hash, Pwd})
    end)end,Workers),
    rec(),
    ok.

wait_for_pwd(0) -> ok;
wait_for_pwd(N) ->
    {ok, {found_password, Hash, Pwd}} = ts:in(task_space, {found_password, any, any}),
    global:whereis_name(sup)!{got_pwd, Hash, Pwd},
    wait_for_pwd(N-1).

rec() ->
    receive
        M -> io:format("~p~n", [M]), rec()
    end.