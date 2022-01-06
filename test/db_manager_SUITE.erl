-module(db_manager_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    create_new_space_test/1,
    ensure_started_test/1,
    ensure_stopped_test/1,
    create_schema_test/1,
    delete_schema_test/1,
    init_tables_test/1
]).

all() -> [{group, all_tests}].

groups() -> [
    {all_tests, [shuffle], [{group, public},{group,internals}]},
    {public, [shuffle], [create_new_space_test]},
    {internals,[shuffle], [ensure_started_test, ensure_stopped_test,
        create_schema_test, delete_schema_test, init_tables_test]}
].

init_per_testcase(create_new_space_test, Config) ->
    logger:start_link(),
    db_manager:start_link(),
    Config;
init_per_testcase(ensure_started_test, Config) ->
    mnesia:stop(),
    Config;
init_per_testcase(ensure_stopped_test, Config) ->
    mnesia:start(),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(create_new_space_test, _) -> 
    db_manager:stop_link(),
    ok;
end_per_testcase(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%
% public API test cases
%%%%%%%%%%%%%%%%%%%%%%%

create_new_space_test(_Config) ->
    ok = db_manager:create_new_space().

%%%%%%%%%%%%%%%%%%%%%
% Internal test cases
%%%%%%%%%%%%%%%%%%%%%

ensure_started_test(_Config) ->
    no = mnesia:system_info(is_running),
    db_manager:ensure_started(),
    yes = mnesia:system_info(is_running),
    ok.

ensure_stopped_test(_Config) ->
    yes = mnesia:system_info(is_running),
    db_manager:ensure_stopped(),
    no = mnesia:system_info(is_running),
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
    % init_tables succed
    db_manager:delete_schema(),
    db_manager:ensure_started(),
    db_manager:create_schema(),
    ok = db_manager:init_tables(),
    Tables =mnesia:system_info(tables),
    ?assert(lists:member(schema, Tables)),
    ?assert(lists:member(tuples_table, Tables)),
    %init_tables fails
    db_manager:ensure_stopped(),
    db_manager:delete_schema(),
    db_manager:ensure_started(),
    {error, _} = db_manager:init_tables(),
    ok.
