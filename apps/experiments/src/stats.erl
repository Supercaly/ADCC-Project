-module(stats).

-export([
    process_dir/1
]).

process_dir(Dir) -> 
    {ok, Files} = file:list_dir(Dir),
    Logs = lists:filter(fun(E) -> string:equal(string:right(E, 8), "-log.txt") end, Files),
    Processed = lists:flatten(lists:map(fun(E) -> process_log_file(Dir,E) end, Logs)),
    HasError = lists:any(fun ({error,_}) -> true; (_) -> false end, Processed),
    if 
        HasError -> io:format("Error processing logs '~p':~n", [Dir]);
        true -> 
            FileName = string:concat(string:concat("all_", filename:basename(Dir)), ".csv"),
            file:write_file(filename:join([Dir, FileName]), format_statistics(Processed))
    end.

process_log_file(Dir, File) -> 
    Path = filename:join([Dir,File]),
    [Node,_] = string:tokens(File, "@"),
    case parse_file(Path) of
        {error, Reason} -> {error, Reason};
        Data -> lists:map(fun({K,V}) -> {list_to_atom(Node), K, V} end, Data)
    end.

parse_file(File) ->
    case file:read_file(File) of
        {ok, Data} -> 
            Lines = lists:filter(fun(E) -> byte_size(E) =/= 0 end, binary:split(Data, <<"\n">>, [global])),
            lists:map(fun(Line) ->
                [Event, Time] = string:tokens(binary_to_list(Line), [$\s]),
                {list_to_atom(Event), list_to_integer(Time)}
            end, Lines);
        _Error -> _Error
    end.

format_statistics(Stats) -> 
    lists:map(fun({N,K,V}) -> io_lib:format("~p, ~p, ~p~n",[N,K,V]) end, Stats).