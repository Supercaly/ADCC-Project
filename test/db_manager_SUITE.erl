-module(db_manager_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2
]).

-export([
    create_new_space_test/1,
    add_node_to_space_test/1,
    remove_node_from_space_test/1,
    list_nodes_test/1,
    ensure_started_test/1,
    ensure_stopped_test/1,
    wait_for_test/1,
    create_schema_test/1,
    delete_schema_test/1,
    init_tables_test/1,
    ensure_tables_test/1,
    revert_db_test/1
]).

all() -> [{group, all_tests}].

groups() -> [
    {all_tests, [shuffle], [{group, publics},{group,internals}]},
    {publics, [shuffle], [create_new_space_test, add_node_to_space_test, 
        remove_node_from_space_test, list_nodes_test]},
    {internals,[shuffle], [create_schema_test, delete_schema_test, init_tables_test,
        ensure_tables_test, ensure_stopped_test, wait_for_test, ensure_started_test,
        revert_db_test]}
].

init_per_group(publics, Config) -> 
    gen_event:start({local, logger_event_manager}),
    gen_event:add_handler(logger_event_manager, logger, []),
    gen_server:start({local, db_manager}, db_manager, [], []),
    Config;
init_per_group(_, Config) -> Config.

end_per_group(publics, _) -> 
    gen_server:stop(db_manager),
    gen_event:stop(logger_event_manager),
    ok;
end_per_group(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%
% public API test cases
%%%%%%%%%%%%%%%%%%%%%%%

create_new_space_test(_Config) ->
    ok = db_manager:create_new_space(),
    ?assert(lists:member(tuples_table, mnesia:system_info(tables))),
    IsRunning = mnesia:system_info(is_running),
    ?assert(IsRunning == yes orelse IsRunning == starting),
    ok.

add_node_to_space_test(_Config) -> 
    % TODO: Find a way to test this method.
    ok.

remove_node_from_space_test(_Config) -> 
    % TODO: Find a way to test this method.
    ok.

list_nodes_test(_Config) -> 
    {ok, Nodes} = db_manager:list_nodes(),
    ?assert(1 == lists:foldl(fun (_, Acc) -> Acc +1 end, 0, Nodes)),
    ok.

%%%%%%%%%%%%%%%%%%%%%
% Internal test cases
%%%%%%%%%%%%%%%%%%%%%

ensure_started_test(_Config) ->
    mnesia:stop(),
    no = mnesia:system_info(is_running),
    db_manager:ensure_started(),
    yes = mnesia:system_info(is_running),
    mnesia:stop(),
    ok.

ensure_stopped_test(_Config) ->
    mnesia:start(),
    yes = mnesia:system_info(is_running),
    db_manager:ensure_stopped(),
    no = mnesia:system_info(is_running),
    ok.

wait_for_test(_Config) ->
    mnesia:start(),
    % wait_for start succed
    ok = db_manager:wait_for(start),
    {error, _} = db_manager:wait_for(stop),
    % wait_for stop succed
    mnesia:stop(),
    ok = db_manager:wait_for(stop),
    {error, _} = db_manager:wait_for(start),
    % wait_for not supported event
    ?assertError(function_clause, db_manager:wait_for(unknown)),
    ok.

create_schema_test(_Config) ->
    % create_schema succed
    db_manager:ensure_started(),
    ok = db_manager:create_schema(),
    % create_schema succed even if there's already a schema
    ok = db_manager:create_schema(),
    % create_schema fails when db_manager is stopped
    db_manager:ensure_stopped(),
    {error, _} = db_manager:create_schema(),
    ok.

delete_schema_test(_Config) ->
    % delete_schema succed when db_manager is stopped
    db_manager:ensure_stopped(),
    ok = db_manager:delete_schema(),
    % delete_schema fails when db_manager is started
    db_manager:ensure_started(),
    {error, _} = db_manager:delete_schema(),
    ok.

init_tables_test(_Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    % init_tables succed
    db_manager:ensure_started(),
    db_manager:create_schema(),
    % TODO: make sure this test passes
    io:format("~p", [mnesia:info()]),
    ok = db_manager:init_tables(),
    Tables =mnesia:system_info(tables),
    ?assert(lists:member(schema, Tables)),
    ?assert(lists:member(tuples_table, Tables)),
    %init_tables fails
    db_manager:ensure_stopped(),
    {error, _} = db_manager:init_tables(),
    ok.

ensure_tables_test(_Config) ->
    db_manager:ensure_started(),
    db_manager:create_schema(),
    db_manager:init_tables(),
    ok = db_manager:ensure_tables(),
    db_manager:ensure_stopped(),
    {error, _} = db_manager:ensure_tables(),
    ok.

revert_db_test(_Config) ->
    db_manager:ensure_started(),
    db_manager:create_schema(),
    db_manager:init_tables(),
    ?assert(lists:member(tuples_table, mnesia:system_info(tables))),
    db_manager:revert_db(),
    ?assertNot(lists:member(tuples_table, mnesia:system_info(tables))),
    ok.