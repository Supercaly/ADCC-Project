-module(ts).

-export([start/0, stop/0]).

-export([in/2, in/3, new/1, out/2, rd/2, rd/3]).

-export([addNode/2, nodes/1, removeNode/2]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Start the ts application on the node.
start() -> 
    case application:start(ts_app) of
        ok -> ok;
        {error, {already_started, ts_app}} -> ok;
        Other -> Other
    end.

% Stop the ts application the node.
stop() -> 
    case application:stop(ts_app) of
        ok -> stopped;
        {error, {not_started, ts_app}} -> stopped;
        Other -> Other
    end.

% Create a new tuple space with given name.
new(Name) -> ok.

% Return a tuple matching given pattern and removes 
% it from the tuple space.
in(Ts, Pattern) -> ok.

% Return a tuple matching given pattern and removes 
% it from the tuple space. 
% Return error if there's no matching tuple after timeout.
in(Ts, Pattern, Timeout) -> ok.

% Return a tuple matching given pattern. 
rd(Ts, Pattern) -> ok.

% Return a tuple matching given pattern. 
% Return error if there's no matching tuple after timeout.
rd(Ts, Pattern, Timeout) -> ok.

% Add given tuple to the tuple space.
out(Ts, Tuple) -> ok.

% Add given node to the tuple space.
addNode(Ts, Node) -> ok.

% Remove given node from the tuple space.
removeNode(Ts, Node) -> ok.

% Return a list of all nodes connected to the tuple space.
nodes(Ts) -> ok.

