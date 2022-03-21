-module(experiments).

-export([main/1]).

main(Arg) ->
    io:format("Hello from experiments ~p~n", [Arg]),
    ts:start().