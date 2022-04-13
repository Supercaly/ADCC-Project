-module(stats).

-export([
    process_dir/1,
    do_format_statistics/1
]).

process_dir(Dir) -> 
    {ok, Files} = file:list_dir(Dir),
    Logs = lists:filter(fun(E) -> string:equal(string:right(E, 8), "-log.txt") end, Files),
    Processed = lists:map(fun(E) -> process_log_file(Dir,E) end, Logs),
    file:write_file(filename:join([Dir, "stats.txt"]),format_statistics(Processed)).

process_log_file(Dir, File) -> 
    Path = filename:join([Dir,File]),
    [Node,_] = string:tokens(File, "@"),
    case parse_file(Path) of
        {error, Reason} -> {error, Reason};
        Data -> {list_to_atom(Node), lists:map(fun({Key, Val}) ->
            Vals = lists:map(fun(E) -> binary_to_integer(list_to_binary(E)) end, Val),
            Mean = lists:foldl(fun(E,A) -> A+E end, 0, Vals) / length(Vals),
            Std = math:sqrt(lists:foldl(fun(E,A) -> A+((E-Mean)*(E-Mean)) end, 0, Vals) / length(Vals)),
            {Key, #{mean => Mean, std => Std}}
        end, Data)}
    end.

parse_file(File) ->
    case file:read_file(File) of
        {ok, Data} -> 
            Lines = lists:filter(fun(E) -> byte_size(E) =/= 0 end, binary:split(Data, <<"\n">>, [global])),
            MappedLines = lists:map(fun(Line) ->
                [Event,_,_,Time] = string:tokens(binary_to_list(Line), [$\s]),
                {Event, Time}
            end, Lines),
            maps:to_list(lists:foldl(fun({Key, Val}, Map)->
                maps:put(list_to_atom(Key), maps:get(list_to_atom(Key), Map, [])++[Val], Map) 
            end, maps:new(), MappedLines));
        _Error -> _Error
    end.

format_statistics(Stats) -> 
    lists:map(fun stats:do_format_statistics/1, Stats).

do_format_statistics({Name, Stat}) when is_list(Stat) -> 
    StatFmt = format_statistics(Stat),
    io_lib:format("~p~n",[Name]) ++ StatFmt;
do_format_statistics({Name, Stat}) -> 
    StatFmt = maps:fold(fun(Key, Val, Acc) ->
        Acc ++ io_lib:format("    ~p: ~p~n",[Key, Val])
    end, [], Stat),
    io_lib:format("  ~p~n",[Name]) ++ StatFmt.