-module(nodelib).

-export([
    spawn_node/1,
    spawn_nodes/2,
    run_on_node/2
]).

-spec spawn_node(Name :: atom()) -> node().
spawn_node(Name) ->
    Args = " -pa erl -pa _build\\default\\lib\\experiments\\ebin -pa _build\\default\\lib\\ts_app\\ebin -secret cookie ",
    {ok, Node} = slave:start_link(localhost,Name,Args),
    pong = net_adm:ping(Node),
    rpc:call(Node, mnesia, delete_schema, [[Node]]),
    rpc:call(Node, ts, start, []),
    Node.

-spec spawn_nodes(NMaster :: integer(), NWorker :: integer()) -> {[node()], [node()]}.
spawn_nodes(NMaster, NWorker) ->
    Masters = lists:map(fun(NodeInt) ->
        Name = "master"++integer_to_list(NodeInt),
        spawn_node(Name)
    end, lists:seq(0, NMaster-1)),
    Workers = lists:map(fun(NodeInt) ->
        Name = "worker"++integer_to_list(NodeInt),
        spawn_node(Name)
    end, lists:seq(0, NWorker-1)),
    {Masters, Workers}.

-spec run_on_node(Node :: atom, Fun :: fun()) -> ok.
run_on_node(Node, Fun) ->
    spawn(Node, Fun),
    ok.