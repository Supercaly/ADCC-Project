-module(db_monitor).

-behaviour(gen_server).

% public exports
-export([
    start_link/0,
    call/0,
    cast/0,
    multicast/0]).

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
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

call() -> ok.
cast() -> ok.
multicast() -> ok.

% init/1 callback function from gen_server
% Note: the state of the gen_server is not used (always []).
init([]) -> 
    process_flag(trap_exit, true),
    ets:new(nodes_ets_table, [set, protected, named_table]),
    ets:new(tuples_ets_table, [bag, protected, named_table]),
    io:format("~p starting at ~p\n", [?MODULE, self()]),
    {ok, []}.

% handle_call/3 callback from gen_server
% Handle messages in the form: 
%   list_nodes |
% Reply with: 
%   {ok, Nodes} | 
%   {error, Reason}
handle_call(list_nodes, _From, _State) ->
    Nodes = do_list_all_nodes(),
    {reply, {ok, Nodes}, _State};
handle_call(Msg, From, State) ->
    % TODO: Remove this function case
    io:format("Generic call: ~p form ~p\n", [Msg, From]),
    {reply, {error, {badarg, "No action found with given name"}}, State}.

% handle_cast/2 callback from gen_server
% Handle messages in the form:
%   {add_node, Node}        |
%   {remove_node, Node}     |
%   {new_tuple, Tuple}      |
%   {sync_tuple, [Tuple]}   |
%   stop
handle_cast({add_node, Node}, _State) ->
    % TODO: Manage adding already existing nodes
    do_add_node(Node),
    io:format("New node added ~p\n", [Node]),
    {noreply, []};
handle_cast({remove_node, Node}, _State) ->
    % TODO: Manage removing unexisting nodes
    do_remove_node(Node),
    io:format("Node ~p removed\n", [Node]),
    {noreply, []};
handle_cast({new_tuple, Tuple}, _State) ->
    do_add_tuple(Tuple),
    io:format("New tuple ~p added\n", [Tuple]),
    {noreply, []};
handle_cast({sync_tuple, Tuples}, _State) ->
    do_sync_db(Tuples),
    io:format("Sync tuples with db ~p\n", [Tuples]),
    {noreply, []};
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

% List all nodes in the nodes db.
do_list_all_nodes() ->
    ets:safe_fixtable(nodes_ets_table, true),
    FirstKey = ets:first(nodes_ets_table),
    Nodes = do_list_all_nodes_rec(FirstKey, [FirstKey]),
    ets:safe_fixtable(nodes_ets_table, false),
    Nodes.

do_list_all_nodes_rec('$end_of_table', ['$end_of_table'|Acc]) ->
    Acc;
do_list_all_nodes_rec(CurrentKey, Acc) ->
    NextKey = ets:next(nodes_ets_table, CurrentKey),
    do_list_all_nodes_rec(NextKey, [NextKey|Acc]).

% Add a node to the nodes db.
% Note: if the node already exist this method does nothing
do_add_node(Node) ->
    ets:insert_new(nodes_ets_table, {Node}).

% Remove a node from the nodes db.
% Note: if the node already doesn't exist this method does nothing
do_remove_node(Node) ->
    ets:delete(nodes_ets_table, Node).

% Add a tuple to the tuples db.
% TODO: Use an unique key for every tuple to avoid duplication (maybe hash?)
do_add_tuple(Tuple) ->
    ets:insert(tuples_ets_table, {key,Tuple}).

% Sync the db with the given new version.
% TODO: Figure out how to avoid deleting all db and coping all data
do_sync_db(Tuples) ->
    ets:delete_all_objects(tuples_ets_table),
    ets:insert(tuples_ets_table, Tuples).