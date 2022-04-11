-module(experiments).

-export([main/1]).

usage(Io) ->
    io:format(Io, "Usage: experiments [OPTIONS] <SUBCOMMAND>~n", []),
    io:format(Io, "  OPTIONS:~n", []),
    io:format(Io, "    -h           print this help message:~n", []),
    io:format(Io, "~n", []),
    io:format(Io, "  SUBCOMMAND:~n", []),
    io:format(Io, "    pwd <n-worker> <n-pwd>               run the 'password search' case study~n", []),
    io:format(Io, "    matrix <n-worker> <matrix-size>      run the 'matrix multiplication' case study~n", []),
    io:format(Io, "~n", []).

main(["pwd", NMasters, NWorkers, NPwds]) ->
    pwd_search:pwd_search_task(list_to_integer(NMasters), 
        list_to_integer(NWorkers), 
        list_to_integer(NPwds));    
main(["matrix", NWorkers, Size]) ->
    matrices:matrix_multiplication_task(list_to_integer(NWorkers), list_to_integer(Size));
main(["-h"]) ->
    usage(standard_io);
main(_) ->
    usage(standard_error),
    io:format(standard_error, "Missing arguments!~n", []).