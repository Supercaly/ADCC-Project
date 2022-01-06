-module(db_manager).

-behaviour(gen_server).

-export([
    add_node_to_space/1,
    create_new_space/0,
    list_nodes/0,
    remove_node_from_space/1,
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
    ensure_started/0, 
    ensure_stopped/0, 
    create_schema/0,
    delete_schema/0,
    init_tables/0
]).
-endif.

-import(logger, [logi/1,logw/1,loge/1]).

% Creates a new tuple space local to this node.
-spec create_new_space() -> ok | {error, term()}.
create_new_space() ->
    call(create_space).

% Adds the given node to this node's tuple space.
-spec add_node_to_space(node()) -> ok | {error, term()}.
add_node_to_space(Node) -> 
    remote_call(Node, {add_to_space, node()}).

% Remove the given node from this node's tuple space.
-spec remove_node_from_space(node()) -> ok | {error, term()}.
remove_node_from_space(Node) -> 
    remote_call(Node, remove_from_space).

% Returns a list of all nodes in this tuple space.
-spec list_nodes() -> {ok, [node()]} | {error, term()}.
list_nodes() -> 
    call(list_nodes).

% Start an instance of db.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Stop the instace of db.
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
    case ensure_started() of
        ok -> 
            logi("db_manager: initialized"),
            {ok, []};
        {error, Reason} -> 
            loge({"db_manager: error initializing", Reason}),
            {stop, Reason}
    end.

% handle_call/3 callback from gen_server.
% Receives messages:
%   create_space          |
%   {add_to_space, Nodes} |
%   remove_from_space     |
%   stop
%
% Responds with:
%   ok              | 
%   {ok, Nodes}     |
%   {error, Reason}
handle_call(create_space, _From, _State) ->
    % Create a new tuple space
    Res = try 
        do_create_new_space(),
        logi("db_manager: new space created"),
        ok
    catch
        error:{badmatch, Error} -> 
            loge("db_manager: error creating new space"),
            revert_db(),
            Error
    end,
    {reply, Res, _State};
handle_call({add_to_space, OtherNode}, _From, _State) when is_atom(OtherNode) ->
    % Add this node to given space
    Res = try 
        do_add_node(OtherNode),
        logi("db_manager: node added to space"),
        ok
    catch
        error:{badmatch, Error} ->
            loge({"db_manager: error adding node to space", node(), OtherNode}),
            revert_db(),
            Error
    end,
    {reply, Res, _State};
handle_call(remove_from_space, _From, _State) ->
    % Remove this node from the space he's in
    Res = try 
        do_remove_node(),
        logi("db_manager: node removed from space"),
        ok
    catch
        error:{badmatch, Error} -> 
            loge("db_manager: error removing node from space"),
            revert_db(),
            Error
    end,
    {reply, Res, _State};
handle_call(list_nodes, _From, _State) ->
    % List all nodes in the space
    Nodes = list_connected_nodes(),
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
    % TODO: Determine what happens when the db is terminated
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
ensure_started() -> 
    mnesia:start(),
    wait_for(start).

% Ensures mnesia db is not running.
% Returns:
%   ok | {error, Reason}
ensure_stopped() -> 
    mnesia:stop(),
    wait_for(stop).

% Wait for mnesia db to start/stop.
% Returns:
%   ok | {error, Reason}
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

% Creates a mnesia scheme as disc_copies.
% Returns:
%   ok | {error, Reason}
create_schema() -> 
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _, _}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

% Deletes a mnesia scheme.
% Returns:
%   ok | {error, Reason}
delete_schema() -> 
    mnesia:delete_schema([node()]).

% Init all the tables for the tuple space.
% Returns:
%   ok | {error, Reason}
init_tables() -> 
    case mnesia:create_table(
        tuples_table, 
        [
            {type, bag}, 
            {disc_copies, [node()]}
        ]) of
            {atomic, ok} -> ok;
            {aborted, Reason} -> {error, Reason}
    end.

% Ensure all the tables are loaded before using them.
% Returns:
%   ok | {error, Reason}
ensure_tables() -> 
    case mnesia:wait_for_tables([tuples_table], 2000) of
        ok -> ok;
        {error, Reason} -> {error, Reason};
        {timeout, _} -> {error, ensure_tables_timeout}
    end.

% Copies all the tables from the space.
% Returns:
%   ok | {error, Reason}
copy_tables() -> 
    case mnesia:add_table_copy(tuples_table, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, tuples_table, _}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

% Connects this node to the given space.
% Returns:
%   ok | {error, Reason}
connect(Node) when is_atom(Node) -> 
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, connection_failed};
        {error, Reason} -> {error, Reason}
    end.

% In case of an error reverts the mnesia db
% to a consistent state.
revert_db() ->
    ensure_stopped(),
    delete_schema(),
    ensure_started().

% List all nodes connected to the current tuple space.
% Returns:
%   [Node]
list_connected_nodes() ->
    mnesia:system_info(running_db_nodes).

% Create a new tuple space no matter if we are in one already.
% This function will return ok or throw a badmatch
% error if any of his operations goes wrong.
do_create_new_space() ->
    ok = ensure_stopped(),
    ok = delete_schema(),
    ok = ensure_started(),
    ok = create_schema(),
    ok = init_tables(),
    ok = ensure_tables().

% Remove old tuple space if exist and join the new one 
% coping all the tables.
% This function will return ok or throw a badmatch
% error if any of his operations goes wrong.
do_add_node(Node) when is_atom(Node) ->
    ok = ensure_stopped(),
    ok = delete_schema(),
    ok = ensure_started(),
    ok = connect(Node),
    ok = create_schema(),
    ok = copy_tables(),
    ok = ensure_tables().

% Deleting the schema it's the only thing in order 
% to exit from the tuple space.
% This function will return ok or throw a badmatch
% error if any of his operations goes wrong.
do_remove_node() ->
    ok = ensure_stopped(),
    ok = delete_schema(),
    ok = ensure_started().

% Makes a call the the local db gen_server.
% Returns:
%   GenServerResponse | {error, db_not_running}
call(Msg) ->
    case whereis(?MODULE) of
        undefined -> {error, db_not_running};
        _Pid -> gen_server:call(?MODULE, Msg)
    end.

% Makes a call the the remote db gen_server on given node.
% Returns:
%   RemoteGenServerResponse | {error, db_not_running}
% Note: In order to work the local gen_server must be started as well.
remote_call(RemoteNode, Msg) ->
    case whereis(?MODULE) of
        undefined -> {error, db_not_running};
        _Pid -> gen_server:call({?MODULE, RemoteNode}, Msg)
    end.