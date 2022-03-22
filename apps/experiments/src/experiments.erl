-module(experiments).

-export([main/1]).

main([NMaster, NWorker, NPwd]) ->
    
    ts:start(),

    {Masters, Workers} = nodelib:spawn_nodes(list_to_integer(NMaster), list_to_integer(NWorker)),
    pwd_search:init_spaces(),
    Hashes = pwd_search:populate_pwd(list_to_integer(NPwd)),

    lists:foreach(fun(Node) ->
        nodelib:run_on_node(Node, fun() ->
            pwd_search:master_task(Hashes)
        end)
    end, Masters),

    lists:foreach(fun(Node) ->
        nodelib:run_on_node(Node, fun() ->
            pwd_search:worker_task()
        end)
    end, Workers),

    ok.