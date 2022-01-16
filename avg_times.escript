#! /ust/local/bin/escript
%%! -pa ebin -sname master@localhost -secret cookie

-export([main/1]).

main(_Arg) ->
    Slave1 = start_node(slave1),
    Slave2 = start_node(slave2),

    start_ts(Slave1),
    start_ts(Slave2),
    
    case rpc:call(Slave1,ts,new,[space1]) of
        ok -> 
            rpc:call(Slave1,ts,addNode,[space1, Slave2]);
        {error,{space_already_exists,_}} -> ok;
        Error -> 
            io:format("~p\n", [Error]),
            halt(1)
    end,
    
    % Compute mean out times
    Outs = time_out(Slave1,10000),
    Outs ++ time_out(Slave2,10000),
    MeanOutTime = lists:foldl(fun(E,Acc) -> Acc + E end, 0, Outs) / length(Outs),
    % Compute mean rd times
    Rds = time_rd(Slave1,10000),
    Rds ++ time_rd(Slave2,10000),
    MeanRdTime = lists:foldl(fun(E,Acc) -> Acc + E end, 0, Rds) / length(Rds),
    % Compute mean in times
    Ins = time_in(Slave1,10000),
    Ins ++ time_in(Slave2,10000),
    MeanInTime = lists:foldl(fun(E,Acc) -> Acc + E end, 0, Ins) / length(Ins),
    % Compute mean node down times
    NodesDowns = time_node_down([Slave1,Slave2],100),
    NodesTime = lists:foldl(fun(E,Acc) -> Acc + E end, 0, NodesDowns) / length(NodesDowns),
    
    io:format("#############################\n"),
    io:format(" Avg times\n"),
    io:format("     in          : ~.2f ms\n",[MeanInTime/1000]),
    io:format("     rd          : ~.2f ms\n",[MeanRdTime/1000]),
    io:format("     out         : ~.2f ms\n",[MeanOutTime/1000]),
    io:format("     node recover: ~.2f ms\n",[NodesTime/1000]),
    io:format("#############################\n").

start_node(Name) ->
    NodeArgs = " -pa ebin -secret cookie ",
    {ok, Node} = slave:start_link(localhost,Name,NodeArgs),
    pong = net_adm:ping(Node),
    rpc:call(Node,mnesia,delete_schema,[[Node]]),
    Node.
    
start_ts(Node) -> rpc:call(Node, ts, start, []).

time_out(_,0) -> [];
time_out(Node,Times) ->
    TimesList = time_out(Node, Times-1),
    {Time,_} = rpc:call(Node,timer,tc,[ts,out,[space1,{test,out,Node,Times}]]),
    TimesList++[Time].

time_rd(_,0) -> [];
time_rd(Node,Times) ->
    TimesList = time_rd(Node, Times-1),
    {Time,_} = rpc:call(Node,timer,tc,[ts,rd,[space1,{any,any,Node,Times},10]]),
    TimesList++[Time].

time_in(_,0) -> [];
time_in(Node,Times) ->
    TimesList = time_in(Node, Times-1),
    {Time,_} = rpc:call(Node,timer,tc,[ts,in,[space1,{any,any,Node,Times},10]]),
    TimesList++[Time].

time_node_down(_Nodes, 0) when length(_Nodes) >= 1 -> [];
time_node_down(Nodes, Times) when length(Nodes) >= 1 ->
    lists:foreach(fun(N) -> rpc:call(N, ts, stop, []) end, Nodes),
    ShuffledNodes = [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- Nodes])],
    CurrentTimesList = lists:map(fun(N) -> 
        {Time,_} = rpc:call(N, timer,tc,[ts, start, []]),
        Time
    end, ShuffledNodes),
    TimesList = time_node_down(Nodes,Times-1),
    TimesList++CurrentTimesList.