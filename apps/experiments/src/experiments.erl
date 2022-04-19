-module(experiments).

-export([main/1]).

usage(Io) ->
    io:format(Io, "Usage: experiments [OPTIONS] <SUBCOMMAND>~n", []),
    io:format(Io, "  OPTIONS:~n", []),
    io:format(Io, "    -h           print this help message:~n", []),
    io:format(Io, "~n", []),
    io:format(Io, "  SUBCOMMAND:~n", []),
    io:format(Io, "    pwd <n-nodes> <workers-per-node> <n-pwd>             run the 'password search' case study~n", []),
    io:format(Io, "    matrix <n-nodes> <workers-per-node> <matrix-size>    run the 'matrix multiplication' case study~n", []),
    io:format(Io, "~n", []).

main(["pwd", NNodes, WorkersPerNode, NPwds]) ->
    pwd_search:pwd_search_task(list_to_integer(NNodes), list_to_integer(WorkersPerNode), list_to_integer(NPwds)),
    stats:process_dir("./profiler/pwd/");
main(["matrix", NNodes, WorkersPerNode, Size]) ->
    matrix_mult:matrix_multiplication_task(list_to_integer(NNodes), list_to_integer(WorkersPerNode), list_to_integer(Size)),
    stats:process_dir("./profiler/matrix/");
main(["-h"]) ->
    usage(standard_io);
main(_) ->
    usage(standard_error),
    io:format(standard_error, "Missing arguments!~n", []).