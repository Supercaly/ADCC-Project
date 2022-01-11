-module(db_manager).

-behaviour(gen_server).

% Public API.
-export([
    add_node_to_space/2,
    create_new_space/1,
    list_nodes_in_space/1,
    remove_node_from_space/2,
    start_link/0,
    stop_link/0
]).

% gen_server callbaks.
-export([
    code_change/3,
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2
]).

% Export internal functions for testing
-ifdef(TEST).
-export([
    addme_to_nodes_unsafe/1,
    addme_to_space/1,
    create_disc_schema/0,
    create_space/1,
    delme_to_nodes_unsafe/1,
    ensure_nodes_table/0,
    ensure_started/0, 
    ensure_stopped/0, 
    init_cluster/0,
    is_node_in_space/2,
    nodes_in_space/1,
    removeme_from_space/1,
    space_exists/1,
    wait_for/1
]).
-endif.

-import(logger, [logi/1,logw/1,loge/1]).

%%%%%%%%%%%%%%
% Custom types
%%%%%%%%%%%%%%

-type unsafe_result() :: {atomic, ok} | {aborted, Reason :: term()}.
-type wait_for_type() :: start | stop.

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Creates a new tuple space local to this node.
% Returns:
%   ok | {error, Reason}
-spec create_new_space(SpaceName :: ts:space()) -> ts:result().
create_new_space(SpaceName) when is_atom(SpaceName) ->
    case whereis(?MODULE) of
        undefined -> {error, db_manager_not_running};
        _Pid -> gen_server:call(?MODULE, {create_space, SpaceName})
    end;
create_new_space(SpaceName) ->
    {error, {badarg, SpaceName}}.

% Adds given node to given tuple space.
% Returns:
%   ok | {error, Reason}
-spec add_node_to_space(Node :: node(), Space :: ts:space()) -> ts:result().
add_node_to_space(Node, Space) when is_atom(Node), is_atom(Space) -> 
    case whereis(?MODULE) of
        undefined -> {error, db_manager_not_running};
        _Pid -> gen_server:call({?MODULE, Node}, {enter_space, Space})
    end;
add_node_to_space(Node, Space) -> 
    {error, {badarg, Node, Space}}.

% Remove given node from given tuple space.
% Returns:
%   ok | {error, Reason}
-spec remove_node_from_space(Node :: node(), Space :: ts:space()) -> ts:result().
remove_node_from_space(Node, Space) when is_atom(Node), is_atom(Space) -> 
    case whereis(?MODULE) of
        undefined -> {error, db_manager_not_running};
        _Pid -> gen_server:call({?MODULE, Node}, {exit_space, Space})
    end;
remove_node_from_space(Node, Space) -> 
    {error, {badarg, Node, Space}}.

% Returns a list of all nodes in this tuple space.
% Returns:
%   {ok, Nodes} | {error, Reason}
-spec list_nodes_in_space(Space :: ts:space()) -> ts:t_result([node()]).
list_nodes_in_space(Space) when is_atom(Space) -> 
    case whereis(?MODULE) of
        undefined -> {error, db_manager_not_running};
        _Pid -> gen_server:call(?MODULE, {nodes_in_space, Space})
    end;
list_nodes_in_space(Space) -> 
    {error, {badarg, Space}}.

% Start an instance of db.
-spec start_link() -> term().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Stop the instace of db.
-spec stop_link() -> stopped.
stop_link() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_server.
init(_Args) ->
    process_flag(trap_exit, true),
    % Start mnesia with the default schema;
    % the schema is manipulated with messages.
    case init_cluster() of
        ok -> 
            logi("db_manager: initialized"),
            {ok, []};
        {error, Reason} -> 
            % TODO(#2): Fix error initializing not_active node
            % starting a node that has some other node where the app is not started causes this
            loge(["db_manager: error initializing", Reason]),
            {stop, Reason}
    end.

% handle_call/3 callback from gen_server.
% Receives messages:
%   {create_space, Name}    |
%   {enter_space, Space}    |
%   {exit_space, Space}     |
%   {nodes_in_space, Space} |
%   stop
%
% Responds with:
%   ok              | 
%   {ok, Nodes}     |
%   {error, Reason}
handle_call({create_space, Name}, _From, _State) ->
    % Create a new space with given name and start a new ts_manager
    Res = case create_space(Name) of
        ok -> ts_supervisor:add_space_manager(Name);
        Error -> Error
    end,
    {reply, Res, _State};
handle_call({enter_space, Space}, _From, _State) ->
    % Add this node to given space and start a new ts_manager
    Res = case addme_to_space(Space) of
        ok -> ts_supervisor:add_space_manager(Space);
        Error -> Error
    end,
    {reply, Res, _State};
handle_call({exit_space, Space}, _From, _State) ->
    % Remove this node form given space and stop his ts_manager
    Res = case removeme_from_space(Space) of
        ok -> ts_supervisor:del_space_manager(Space);
        Error -> Error
    end,
    {reply, Res, _State};
handle_call({nodes_in_space, Space}, _From, _State) ->
    % List all nodes in given space
    Nodes = nodes_in_space(Space),
    {reply, {ok, Nodes}, _State};
handle_call(stop, _From, _State) ->
    {stop, normal, stopped, _State};
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

% handle_cast/2 callback from gen_server.
handle_cast(_Msg, _State) ->
    logi({"db_manager: cast message received", _Msg}),
    {noreply, _State}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) ->
    logi({"db_manager: info message received", _Info}),
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Reason, _State) ->
    % TODO(#3): Determine what happens when the db is terminated
    ensure_stopped(),
    logi({"db_manager: terminated", _Reason}),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) ->
    logi({"db_manager: code changed", _OldVsn}),
    {ok, _State}.

%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%

% Ensures mnesia db is running.
% Returns:
%   ok | {error, Reason}
-spec ensure_started() -> ts:result().
ensure_started() -> 
    mnesia:start(),
    wait_for(start).

% Ensures mnesia db is not running.
% Returns:
%   ok | {error, Reason}
-spec ensure_stopped() -> ts:result().
ensure_stopped() -> 
    mnesia:stop(),
    wait_for(stop).

% Wait for mnesia db to start/stop.
% Returns:
%   ok | {error, Reason}
-spec wait_for(wait_for_type()) -> ts:result().
wait_for(start) -> 
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> {error, mnesia_unexpectedly_not_running};
        stopping -> {error, mnesia_unexpectedly_stopping};
        starting -> 
            timer:sleep(1000),
            wait_for(start)
    end;
wait_for(stop) -> 
    case mnesia:system_info(is_running) of
        no -> ok;
        yes -> {error, mnesia_unexpectedly_running};
        starting -> {error, mnesia_unexpectedly_starting};
        stopping -> 
            timer:sleep(1000),
            wait_for(stop)
    end.

% Initialize the main cluster with all the nodes 
% connected to the colling one.
% Returns:
%   ok | {error, Reason}
-spec init_cluster() -> ts:result().
init_cluster() ->
    try
        % TODO(#4): Figure out if deleting the schema at every startup is a correct thing to do
        ok = ensure_stopped(),
        ok = mnesia:delete_schema([node()]),
        ok = ensure_started(),
        {ok, _} = mnesia:change_config(extra_db_nodes, nodes()),
        ok = create_disc_schema(),
        ok = ensure_nodes_table(),
        ok = mnesia:wait_for_tables([schema, nodes], 2000),
        ok
    catch
        error:{badmatch, {timeout, Tab}} -> {error, {cannot_find_tables, Tab}};
        error:{badmatch, Error} -> Error
    end.

% Creates a mnesia scheme as disc_copies.
% Returns:
%   ok | {error, Reason}
-spec create_disc_schema() -> ts:result().
create_disc_schema() -> 
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _, _}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

% Ensure the nodes table is present in the current cluster
% every node must have this table so if it's not present we
% create it.
% Returns:
%   ok | {error, Reason}
-spec ensure_nodes_table() -> ts:result().
ensure_nodes_table() ->
    Exist = lists:member(nodes, mnesia:system_info(tables)),
    if 
        not Exist -> 
            case mnesia:create_table(nodes, 
                [{type, bag}, {disc_copies, nodes()++[node()]}]) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, nodes}} -> ok;
                {aborted, Reason} -> {error, Reason}
            end;
        true -> ok
    end.

% Returns true if given space exists in the db.
% Returns:
%   true | false
-spec space_exists(Space :: ts:space()) -> boolean().
space_exists(Space) when is_atom(Space) ->
    lists:member(Space, mnesia:system_info(tables)).

% Create a new tuple space with given name.
% This function returns error if a space with the same
% name already exist.
% Returns:
%   ok | {error, Reason}
-spec create_space(Name :: ts:space()) -> ts:result().
create_space(Name) when is_atom(Name) ->
    Exist = space_exists(Name),
    if
        not Exist -> 
            try
                {atomic, ok} = mnesia:create_table(Name, [{type, bag}, {disc_copies, [node()]}]),
                {atomic, ok} = addme_to_nodes_unsafe(Name),
                ok
            catch
                error:{badmatch, {aborted, Error}} -> {error, Error}
            end;
        true -> {error, {space_already_exists, Name}}
    end.

% Add this node to the given tuple space.
% This function returns error if the node is already in the tuple space
% Returns:
%   ok | {error, Reason}
-spec addme_to_space(Space :: ts:space()) -> ts:result().
addme_to_space(Space) when is_atom(Space) ->
    Exist = space_exists(Space),
    if
        % the space i want to add me exist
        Exist -> 
            IsNodeInSpace = is_node_in_space(node(), Space),
            if
                not IsNodeInSpace -> 
                    % add the node to the space
                    try
                        {atomic, ok} = mnesia:add_table_copy(Space, node(), disc_copies),
                        {atomic, ok} = addme_to_nodes_unsafe(Space),
                        ok
                    catch
                        error:{badmatch,{aborted, Error}} -> {error, Error}
                    end;
                true -> {error, {node_already_in_space, Space}}
            end;
        true -> {error, {space_not_exists, Space}}
    end.

% Remove this node from the given tuple space.
% This function returns error if the node is not in the tuple space
% Returns:
%   ok | {error, Reason}
-spec removeme_from_space(Space :: ts:space()) -> ts:result().
removeme_from_space(Space) when is_atom(Space) ->
    Exist = space_exists(Space),
    if
        Exist ->
            IsNodeInSpace = is_node_in_space(node(), Space),
            if
                IsNodeInSpace ->
                    % remove the node from space
                    try
                        {atomic, ok} = mnesia:del_table_copy(Space, node()),
                        {atomic, ok} = delme_to_nodes_unsafe(Space),
                        ok
                    catch
                        error:{badmatch, {aborted, Error}} -> {error, Error}
                    end;
                true -> {error, {node_not_in_space, Space}}
            end;
        true -> {error, {space_not_exists, Space}}
    end.

% List all nodes connected to the given tuple space.
% Returns:
%   [Node]
-spec nodes_in_space(Space :: ts:space()) -> [node()].
nodes_in_space(Space) when is_atom(Space) ->
    Res = mnesia:transaction(fun() ->
        mnesia:read(nodes, Space)
    end),
    case Res of
        {atomic, Nodes} -> 
            lists:map(fun(E) -> 
                element(3, E)
            end, Nodes);
        {aborted, _} -> []
    end.

% Returns true if the given node is in the given space; false otherwise.
% Returns:
%   true | false
-spec is_node_in_space(Node :: node(), Space :: ts:space()) -> boolean().
is_node_in_space(Node, Space) when is_atom(Node), is_atom(Space) ->
    lists:member(Node, nodes_in_space(Space)).

% Add the current node to the given space inside the nodes shared table.
% This operation is usafe because it resurns unsafe_result like the mnesia ones.
-spec addme_to_nodes_unsafe(Space :: ts:space()) -> unsafe_result().
addme_to_nodes_unsafe(Space) when is_atom(Space) ->
    mnesia:transaction(fun() -> mnesia:write({nodes, Space, node()}) end).

% Remove the current node from the given space inside the nodes shared table.
% This operation is usafe because it resurns unsafe_result like the mnesia ones.
-spec delme_to_nodes_unsafe(Space :: ts:space()) -> unsafe_result().
delme_to_nodes_unsafe(Space) when is_atom(Space) ->
    mnesia:transaction(fun() -> mnesia:delete_object({nodes, Space, node()}) end).
