-module(db).

-behaviour(gen_server).

-export([
    start_link/0,
    stop_link/0
    ]).

% gen_server callbaks.
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
    ]).

% Start an instance of db.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    ensure_started(),
    {ok, []}.

% handle_call/3 callback from gen_server.
handle_call(stop, _From, _State) ->
    {stop, normal, stopped, _State};
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

% handle_cast/2 callback from gen_server.
% Receives messages:
%   {create_space}          |
%   {add_to_space, Nodes}   |
%   {remove_from_space}     |
handle_cast({create_space}, _State) ->
    % Create a new tuple space no matter if we 
    % are in one already.
    ensure_stopped(),
    delete_schema(),
    create_schema(),
    ensure_started(),
    init_tables(),
    ensure_tables(),
    {noreply, _State};
handle_cast({add_to_space, OtherNodes}, _State) ->
    % Remove old tuple space if exist and join the new one
    % coping all the tables.
    ensure_stopped(),
    delete_schema(),
    ensure_started(),
    connect(OtherNodes),
    copy_tables(),
    ensure_tables(),
    {noreply, _State};
handle_cast({remove_from_space}, _State) ->
    % Deleting the schema it's the only thing
    % in order to exit from the tuple space.
    ensure_stopped(),
    delete_schema(),
    ensure_started(),
    {noreply, _State}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) ->
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Reason, _State) ->
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%

% Ensures mnesia db is running.
ensure_started() -> 
    mnesia:start(),
    wait_for(start),
    ok.

% Ensures mnesia db is not running.
ensure_stopped() -> 
    mnesia:stop(),
    wait_for(stop),
    ok.

% Wait for mnesia db to start/stop.
wait_for(start) -> 
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> {error, not_running};
        stopping -> {error, not_running};
        starting -> 
            timer:sleep(1000),
            wait_for(start)
    end;
wait_for(stop) -> 
    case mnesia:system_info(is_running) of
        no -> ok;
        yes -> {error, running};
        starting -> {error, running};
        stopping -> 
            timer:sleep(1000),
            wait_for(stop)
    end.


create_schema() -> 
    mnesia:change_table_copy_type(schema, node(), disc_copies).

delete_schema() -> 
    mnesia:delete_schema([node()]).

init_tables() -> 
    mnesia:create_table(
        tuples_table, 
        [
            {type, bag}, 
            {disc_copies, [node()]}
        ]).

ensure_tables() -> 
    mnesia:wait_for_tables([tuples_table], 2000),
    ok.

copy_tables() -> mnesia:add_table_copy(tuples_table, node(), disc_copies),ok.

connect(Node) -> 
    mnesia:change_config(extra_db_nodes, Node),
    ok.
