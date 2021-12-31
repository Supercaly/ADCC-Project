-module(db_monitor).

-behaviour(gen_server).

% public exports
-export([start_link/0]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

% start_link/0 start a new local instance of db_monitor gen_server.
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, 0, []).

% init/1 callback function from gen_server
% Returns: {ok, {[],[]}}
init(_InitArgs) -> 
    io:format("Init db_monitor\n"),
    {ok, {[],[]}}.

% handle_call/3 callback from gen_server
% Handle messages in the form: 
%   list_nodes |
% Reply with: 
%   {ok, Nodes} | 
%   {error, Reason}
handle_call(list_nodes, _From, _State) ->
    {Nodes,_} = _State,
    {reply, {ok, Nodes}, _State};
handle_call(Msg, From, State) ->
    io:format("Generic call: ~p form ~p\n", [Msg, From]),
    {reply, {error, {badarg, "No action found with given name"}}, State}.

% handle_cast/2 callback from gen_server
% Handle messages in the form:
%   {add_node, Node}        |
%   {remove_node, Node}     |
%   {new_tuple, Tuple}      |
%   {sync_tuple, [Tuple]}   |
%   stop
handle_cast({add_node, Node}, State) ->
    io:format("New node added ~p\n", [Node]),
    {Nodes,Tuples} = State,
    {noreply, {Nodes++[Node],Tuples}};
handle_cast({remove_node, Node}, State) ->
    io:format("Node ~p removed\n", [Node]),
    {Nodes,Tuples} = State,
    {noreply, {Nodes--[Node],Tuples}};
handle_cast({new_tuple, Tuple}, State) ->
    io:format("New tuple ~p added\n", [Tuple]),
    {Nodes,Tuples} = State,
    {noreply, {Nodes,Tuples++[Tuple]}};
handle_cast({sync_tuple, Tuples}, State) ->
    io:format("Sync tuples with db ~p\n", [Tuples]),
    {Nodes,_} = State,
    {noreply, {Nodes,Tuples}};
handle_cast(stop, State) -> {stop, normal, State}.

% handle_info/2 callback from gen_server.
handle_info(Msg, State) -> 
    io:format("info: ~p\n",[Msg]),
    {noreply, State}.

% terminate/2 callback from gen_server.
terminate(Reason, State) -> 
    io:format("~p terminated: ~p, state: ~p\n",[?MODULE,Reason,State]),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, State, _Extra)-> {ok, State}.