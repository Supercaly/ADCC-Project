-module(nodelib).

-export([
    spawn_node/1,
    spawn_nodes/1,
    run_on_node/2
]).

spawn_node(Name) ->
    Args = " -pa erl -pa _build\\default\\lib\\experiments\\ebin -pa _build\\default\\lib\\ts_app\\ebin -secret cookie ",
    {ok, Node} = slave:start_link(localhost,Name,Args),
    pong = net_adm:ping(Node),
    rpc:call(Node, mnesia, delete_schema, [[Node]]),
    Node.

spawn_nodes(N) ->
    lists:map(fun(NodeInt) ->
        Name = "node"++integer_to_list(NodeInt),
        spawn_node(Name)
    end, lists:seq(0, N)).

run_on_node(Node, Fun) ->
    spawn(Node, Fun).