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
    case slave:start_link(HostName, NodeName," -pa `rebar3 path` ") of
        {ok, Node} -> 
            net_adm:ping(Node),
            call_node(Node, code, add_paths, [code:get_path()]),
            Node;
        {error, {already_running, Node}} -> 
            stop_node(Node)
    end.

stop_node(Node) -> 
    slave:stop(Node),
    ok.

call_node(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args).

call_node_after(Node, Module, Function, Args, Time) ->
    timer:apply_after(Time,rpc,cast,[Node, Module, Function, Args]).