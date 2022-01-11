-module(ts).

-export([start/0, stop/0]).

-export([in/2, in/3, new/1, out/2, rd/2, rd/3]).

-export([addNode/2, nodes/1, removeNode/2]).

%%%%%%%%%%%%%%%%
% Exported types
%%%%%%%%%%%%%%%%

-type space() :: atom().
-type result() :: 'ok' | {'error', Reason :: term()}.
-type t_result(Res) :: {'ok', Res} | {'error', Reason :: term()}.

-export_type([
    t_result/1,
    space/0,
    result/0
]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Start the ts application on the node.
-spec start() -> result().
start() -> 
    case application:start(ts_app) of
        ok -> ok;
        {error, {already_started, ts_app}} -> ok;
        Other -> Other
    end.

% Stop the ts application the node.
-spec stop() -> 'stopped' | {'error', term()}.
stop() -> 
    case application:stop(ts_app) of
        ok -> stopped;
        {error, {not_started, ts_app}} -> stopped;
        Other -> Other
    end.

% Create a new tuple space with given name.
-spec new(Name :: atom()) -> t_result(atom()).
new(Name) -> 
    db_manager:create_new_space(Name).

% Return a tuple matching given pattern and removes 
% it from the tuple space.
-spec in(Ts :: space(), Pattern :: tuple()) -> t_result(tuple()).
in(Ts, Pattern) -> 
    ts_manager:perform_in(Ts, Pattern, 'infinity').

% Return a tuple matching given pattern and removes 
% it from the tuple space. 
% Return error if there's no matching tuple after timeout.
-spec in(Ts :: space(), 
    Pattern :: tuple(), 
    Timeout :: timeout()) -> t_result(tuple()).
in(Ts, Pattern, Timeout)-> 
    ts_manager:perform_in(Ts, Pattern, Timeout).

% Return a tuple matching given pattern. 
-spec rd(Ts :: space(), Pattern :: tuple()) -> t_result(tuple()).
rd(Ts, Pattern) -> 
    ts_manager:perform_rd(Ts, Pattern, 'infinity').

% Return a tuple matching given pattern. 
% Return error if there's no matching tuple after timeout.
-spec rd(Ts :: space(), 
    Pattern :: tuple(), 
    Timeout :: timeout()) -> t_result(tuple()).
rd(Ts, Pattern, Timeout) -> 
    ts_manager:perform_rd(Ts, Pattern, Timeout).

% Add given tuple to the tuple space.
-spec out(Ts :: space(), Tuple :: tuple()) -> result().
out(Ts, Tuple) -> 
    ts_manager:perform_out(Ts, Tuple).

% TODO(#7): addNode/removeNode can be called by node not inside the Space, this mechanic is correct?
% Add given node to the tuple space.
addNode(Ts, Node) -> 
    db_manager:add_node_to_space(Node, Ts).

% Remove given node from the tuple space.
removeNode(Ts, Node) -> 
    db_manager:remove_node_from_space(Node, Ts).

% Return a list of all nodes connected to the tuple space.
-spec nodes(Ts :: space()) -> t_result([node()]).
nodes(Ts) -> 
    db_manager:list_nodes_in_space(Ts).

