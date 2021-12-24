-module(ts).

-export([new/1, in/2, in/3, rd/2, rd/3, out/2, addNode/2, removeNode/2, nodes/1]).

% Node management
addNode(TS, Node) -> ok.
removeNode(TS, Node) -> ok.
nodes(TS) -> ok.

% Tuple Space management
new(Name) -> ok.

in(TS, Pattern) -> ok.
in(TS, Pattern, Timeout) -> ok.

rd(TS, Pattern) -> ok.
rd(TS, Pattern, Timeout) -> ok.

out(TS, Tuple) -> ok.

match(L1, L2) -> ok.

tuple2list(Tuple) -> [].