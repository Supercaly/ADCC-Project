-module(experiments).

-export([main/1]).

usage(Io) ->
    io:format(Io, "Usage: experiments [OPTIONS] <SUBCOMMAND>~n", []),
    io:format(Io, "  OPTIONS:~n", []),
    io:format(Io, "    -h           print this help message:~n", []),
    io:format(Io, "~n", []),
    io:format(Io, "  SUBCOMMAND:~n", []),
    io:format(Io, "    pwd          run the 'password search' case study~n", []),
    io:format(Io, "~n", []).

main(["pwd"]) ->
    pwd_search_task(1,2,10);
main(["pwd", NMasters, NWorkers, NPwds]) ->
    pwd_search_task(list_to_integer(NMasters), list_to_integer(NWorkers), list_to_integer(NPwds));    
main(["-h"]) ->
    usage(standard_io);
main(_) ->
    usage(standard_error),
    io:format(standard_error, "Missing arguments!~n", []).

% Perform the password search test case
pwd_search_task(NMasters, NWorkers, NPwds) ->
    nodelib:init_sup(),
    {Masters, Workers} = nodelib:spawn_nodes(NMasters, NWorkers),
    pwd_search:init_spaces(),
    Hashes = pwd_search:populate_pwd(NPwds),

    % TODO: Figure out the termination with 2 or more masters
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

    nodelib:wait_for_nodes(NMasters),
    ok.