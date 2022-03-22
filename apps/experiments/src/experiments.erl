-module(experiments).

-export([main/1]).

main(_Arg) ->
    pwd_search:init_spaces(),
    Hashes = pwd_search:populate_pwd(10),

    [Master|Workers] = nodelib:spawn_nodes(2),

    nodelib:run_on_node(Master, fun() ->
        pwd_search:master_task(Hashes)
    end),
    lists:foreach(fun(Node) ->
        nodelib:run_on_node(Node, fun() ->
            pwd_search:worker_task()
        end)
    end, Workers),
    io:format("Ciao mondo"),
    ok.