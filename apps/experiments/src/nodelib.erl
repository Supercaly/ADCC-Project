-module(nodelib).

-export([
    spawn_node/1,
    spawn_nodes/2,
    run_on_node/2,
    init_sup/0,
    get_supervisor/0,
    wait_for_nodes/1
]).

% Init the supervisor node
-spec init_sup() -> ok.
init_sup() ->
    global:register_name(supervisor, self()),
    ok = mnesia:delete_schema([node()]),
    ok = ts:start(),
    ok.

% Return the pid of the supervisor node
-spec get_supervisor() -> undefined | pid().
get_supervisor() ->
    global:whereis_name(supervisor).

% Spawn a node with given name
-spec spawn_node(Name :: atom()) -> node().
spawn_node(Name) ->
    % TODO: Passing source as -pa can cause problems under windows paths
    Args = " -pa _build/default/lib/experiments/ebin -pa _build/default/lib/ts_app/ebin -secret cookie",
    {ok, Node} = slave:start_link(localhost,Name,Args),
    pong = net_adm:ping(Node),
    ok = rpc:call(Node, mnesia, delete_schema, [[Node]]),
    ok = rpc:call(Node, ts, start, []),
    Node.

% Spawn N master and M worker nodes
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

% Run given function on given node
-spec run_on_node(Node :: atom, Fun :: fun()) -> ok.
run_on_node(Node, Fun) ->
    spawn(Node, Fun),
    ok.

% Wait for the completion of N nodes
-spec wait_for_nodes(N :: integer()) -> ok.
wait_for_nodes(0) -> ok;
wait_for_nodes(N) ->
    receive
        {finished, Node} ->
            io:format("Node '~p' has finish~n", [Node]),
            wait_for_nodes(N-1)
    end.