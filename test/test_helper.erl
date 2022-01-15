-module(test_helper).

-export([
    call_node/4,
    call_node_after/5,
    clear_db_for_test/0,
    start_node/1,
    stop_node/1
]).

%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%

clear_db_for_test() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok.

start_node(NodeName) ->
    {ok, HostName} = inet:gethostname(),
    {ok, Node} = slave:start(HostName, NodeName," -pa ../ebin/ "),
    net_adm:ping(Node),
    Node.

stop_node(Node) -> slave:stop(Node).

call_node(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args).

call_node_after(Node, Module, Function, Args, Time) ->
    timer:apply_after(Time,rpc,cast,[Node, Module, Function, Args]).