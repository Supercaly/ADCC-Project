-module(ts).

start() -> ok.

stop() -> ok.

new(Name) -> ok.

in(Ts, Pattern) -> ok.
in(Ts, Pattern, Timeout) -> ok.
rd(Ts, Pattern) -> ok.
rd(Ts, Pattern, Timeout) -> ok.
out(Ts, Tuple) -> ok.

addNode(Ts, Node) -> ok.
removeNode(Ts, Node) -> ok.
nodes(Ts) -> ok.

