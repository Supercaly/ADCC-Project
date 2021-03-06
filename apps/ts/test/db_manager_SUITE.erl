-module(db_manager_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    end_per_group/2,
    groups/0,
    init_per_group/2
]).

-export([
    create_new_space_test/1,
    add_node_to_space_test/1,
    remove_node_from_space_test/1,
    list_nodes_in_space_test/1,
    addme_to_nodes_unsafe_test/1,
    addme_to_space_test/1,
    create_disc_schema_test/1,
    create_space_test/1,
    delme_to_nodes_unsafe_test/1,
    ensure_nodes_table_test/1,
    ensure_started_test/1, 
    ensure_stopped_test/1, 
    init_cluster_test/1,
    is_node_in_space_test/1,
    nodes_in_space_test/1,
    removeme_from_space_test/1,
    space_exists_test/1,
    wait_for_test/1
]).

all() -> [{group, publics},{group,internals}].

groups() -> [
    {publics, [shuffle,sequence], [create_new_space_test, add_node_to_space_test, 
        remove_node_from_space_test, list_nodes_in_space_test]},
    {internals,[shuffle,sequence], [addme_to_nodes_unsafe_test, addme_to_space_test, create_disc_schema_test,
        create_space_test, delme_to_nodes_unsafe_test, ensure_nodes_table_test, ensure_started_test, 
        ensure_stopped_test, init_cluster_test, is_node_in_space_test, nodes_in_space_test, 
        removeme_from_space_test, space_exists_test, wait_for_test]}
].

init_per_group(publics, Config) -> 
    gen_server:start({local, logger}, logger, [], []),
    Config;
init_per_group(_, Config) -> Config.

end_per_group(publics, _) -> 
    gen_server:stop(logger),
    ok;
end_per_group(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%
% Public API test cases
%%%%%%%%%%%%%%%%%%%%%%%

create_new_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    {ok,_} = gen_server:start({local, db_manager}, db_manager, [], []),
    {ok,_} = supervisor:start_link({local, ts_supervisor}, ts_supervisor, []),
    
    ?assertNot(lists:member(test_space, mnesia:system_info(tables))),
    ?assertEqual(undefined, whereis(test_space)),
    ?assertMatch(ok, db_manager:create_new_space(test_space)),
    ?assert(lists:member(test_space, mnesia:system_info(tables))),
    ?assertNotEqual(undefined, whereis(test_space)),
    
    ?assertMatch({error, {space_already_exists, _}}, db_manager:create_new_space(test_space)),
    gen_server:call(db_manager, stop),
    ?assertMatch({error, db_manager_not_running}, db_manager:create_new_space(test_space2)),
    ?assertMatch({error,{badarg,_}}, db_manager:create_new_space(2)),
    
    ok.

add_node_to_space_test(_Config) -> 
    ok = test_helper:clear_db_for_test(),
    {ok,_} = gen_server:start({local, db_manager}, db_manager, [], []),
    {ok,_} = supervisor:start_link({local, ts_supervisor}, ts_supervisor, []),
    Node = test_helper:start_node(node),
    ok = test_helper:call_node(Node,mnesia,delete_schema,[[Node]]),
    ok = test_helper:call_node(Node,ts,start,[]),
    ok = test_helper:call_node(Node,ts,new,[test_space]),
    
    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(unknown, mnesia:table_info(test_space, storage_type)),
    ok = test_helper:call_node(Node,db_manager,add_node_to_space,[node(),test_space]),
    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(disc_copies, mnesia:table_info(test_space, storage_type)),

    ?assertMatch({error,{node_already_in_space,_,_}},test_helper:call_node(Node,db_manager,add_node_to_space,[node(),test_space])),
    ?assertMatch({error,{node_not_in_space,_,_}},db_manager:add_node_to_space(node(),test_space2)),
    ?assertMatch({error, {badarg,_,_}}, db_manager:add_node_to_space(1, test_space)),
    ?assertMatch({error, {badarg,_,_}}, db_manager:add_node_to_space(node(), [a])),
    
    test_helper:stop_node(Node),
    gen_server:stop(db_manager),

    ok.

remove_node_from_space_test(_Config) -> 
    ok = test_helper:clear_db_for_test(),
    {ok,_} = gen_server:start({local, db_manager}, db_manager, [], []),
    {ok,_} = supervisor:start_link({local, ts_supervisor}, ts_supervisor, []),
    Node = test_helper:start_node(node),
    ok = test_helper:call_node(Node,mnesia,delete_schema,[[Node]]),
    ok = test_helper:call_node(Node,ts,start,[]),
    ok = test_helper:call_node(Node,ts,new,[test_space]),
    ok = test_helper:call_node(Node,db_manager,add_node_to_space,[node(),test_space]),
    
    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(disc_copies, mnesia:table_info(test_space, storage_type)),
    ok = test_helper:call_node(Node,db_manager,remove_node_from_space,[node(),test_space]),
    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(unknown, mnesia:table_info(test_space, storage_type)),

    ?assertMatch({error,{node_not_in_space,_,_}},db_manager:add_node_to_space(node(),test_space2)),
    ?assertMatch({error, {badarg,_,_}}, db_manager:remove_node_from_space(1, test_space)),
    ?assertMatch({error, {badarg,_,_}}, db_manager:remove_node_from_space(node(), [a])),
    
    test_helper:stop_node(Node),
    gen_server:stop(db_manager),
    
    ok.

list_nodes_in_space_test(_Config) -> 
    ok = test_helper:clear_db_for_test(),
    {ok,_} = gen_server:start({local, db_manager}, db_manager, [], []),
    {atomic,ok} = mnesia:create_table(test_space, [{type, bag}]),
    ok = mnesia:dirty_write({nodes, test_space, another_node@host}),

    ?assertMatch({error,{node_not_in_space,_,_}}, db_manager:list_nodes_in_space(test_space)),    
    ok = mnesia:dirty_write({nodes, test_space, node()}),
    {ok, Nodes} = db_manager:list_nodes_in_space(test_space),
    ?assertEqual(2, lists:foldl(fun (_, Acc) -> Acc +1 end, 0, Nodes)),
    
    gen_server:call(db_manager, stop),
    ?assertMatch({error, db_manager_not_running}, db_manager:list_nodes_in_space(test_space)),
    ?assertMatch({error, {badarg,_}}, db_manager:list_nodes_in_space(2)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%
% Internal test cases
%%%%%%%%%%%%%%%%%%%%%

ensure_started_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    
    ?assertMatch(no, mnesia:system_info(is_running)),
    ok = db_manager:ensure_started(),
    ?assertMatch(yes, mnesia:system_info(is_running)),
    
    ok.

ensure_stopped_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    
    ?assertMatch(yes, mnesia:system_info(is_running)),
    ok = db_manager:ensure_stopped(),
    ?assertMatch(no, mnesia:system_info(is_running)),

    ok.

wait_for_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    
    ok = mnesia:start(),
    % wait_for start succed
    ?assertMatch(ok, db_manager:wait_for(start)),
    ?assertMatch({error, _}, db_manager:wait_for(stop)),
    % wait_for stop succed
    stopped = mnesia:stop(),
    ?assertMatch(ok, db_manager:wait_for(stop)),
    ?assertMatch({error, _}, db_manager:wait_for(start)),
    % wait_for not supported event
    ?assertError(function_clause, db_manager:wait_for(unknown)),
    
    ok.

init_cluster_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    
    ?assertNot(lists:member(nodes, mnesia:system_info(tables))),
    ?assertMatch(ok, db_manager:init_cluster()),
    ?assert(lists:member(nodes, mnesia:system_info(tables))),
    
    ok.

create_disc_schema_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(), 

    ?assertEqual(mnesia:table_info(schema, storage_type), ram_copies),
    ?assertMatch(ok, db_manager:create_disc_schema()),
    ?assertEqual(mnesia:table_info(schema, storage_type), disc_copies),
    
    ok.

ensure_nodes_table_test(_Config) -> 
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),

    ?assertMatch(ok, db_manager:ensure_nodes_table()),
    ?assertMatch(ok, db_manager:ensure_nodes_table()),
    
    ok.

space_exists_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:create_table(s1, []),
    {atomic,ok} = mnesia:create_table(s2, []),

    ?assert(db_manager:space_exists(s1)),
    ?assert(db_manager:space_exists(s2)),
    ?assertNot(db_manager:space_exists(s3)),

    ok.

create_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),

    ?assertMatch(ok, db_manager:create_space(test_space)),
    ?assert(lists:member(test_space, mnesia:system_info(tables))),
    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertMatch({error, {space_already_exists, test_space}}, db_manager:create_space(test_space)),
    
    ok.

addme_to_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    Node = test_helper:start_node(node),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),
    ok = test_helper:call_node(Node,mnesia,delete_schema,[[Node]]),
    ok = test_helper:call_node(Node,mnesia,start,[]),
    {ok,_} = test_helper:call_node(Node,mnesia,change_config,[extra_db_nodes,[node()]]),
    {atomic,ok} = test_helper:call_node(Node,mnesia,change_table_copy_type,[schema, Node, disc_copies]),
    {atomic,ok} = test_helper:call_node(Node,mnesia,create_table,[test_space, [{type,bag}]]),

    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(unknown, mnesia:table_info(test_space, storage_type)),
    ?assertMatch(ok, db_manager:addme_to_space(test_space)),
    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(disc_copies, mnesia:table_info(test_space, storage_type)),

    ?assertMatch({error, {node_already_in_space, _, _}}, db_manager:addme_to_space(test_space)),
    ?assertMatch({error, {space_not_exists, _}}, db_manager:addme_to_space(test_space2)),

    test_helper:stop_node(Node),
    
    ok.

removeme_from_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    Node = test_helper:start_node(node),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),
    ok = test_helper:call_node(Node,mnesia,delete_schema,[[Node]]),
    ok = test_helper:call_node(Node,mnesia,start,[]),
    {ok,_} = test_helper:call_node(Node,mnesia,change_config,[extra_db_nodes,[node()]]),
    {atomic,ok} = test_helper:call_node(Node,mnesia,change_table_copy_type,[schema, Node,disc_copies]),
    {atomic,ok} = test_helper:call_node(Node,mnesia,create_table,[test_space, [{type,bag}]]),
    ok = db_manager:addme_to_space(test_space),

    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(disc_copies, mnesia:table_info(test_space, storage_type)),
    ?assertMatch(ok, db_manager:removeme_from_space(test_space)),
    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    ?assertEqual(unknown, mnesia:table_info(test_space, storage_type)),

    ?assertMatch({error, {node_not_in_space, _, _}}, db_manager:removeme_from_space(test_space)),
    ?assertMatch({error, {space_not_exists, _}}, db_manager:removeme_from_space(test_space2)),

    test_helper:stop_node(Node),

    ok.

nodes_in_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),
    ok = mnesia:dirty_write({nodes, test_space, node()}),
    ok = mnesia:dirty_write({nodes, test_space, another_node@host}),

    Nodes = db_manager:nodes_in_space(test_space),
    ?assert(lists:member(node(), Nodes)),
    ?assert(lists:member(another_node@host, Nodes)),
    ?assertNot(lists:member(different_node@host, Nodes)),
    
    ok.

is_node_in_space_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),
    ok = mnesia:dirty_write({nodes, test_space, node()}),

    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertNot(db_manager:is_node_in_space(another_node@host, test_space)),
    
    ok.

addme_to_nodes_unsafe_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),

    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    ?assertMatch({atomic, ok}, db_manager:addme_to_nodes_unsafe(test_space)),
    ?assert(db_manager:is_node_in_space(node(), test_space)),
    {atomic,ok} = mnesia:delete_table(nodes),
    ?assertMatch({aborted, _}, db_manager:addme_to_nodes_unsafe(test_space)),
    
    ok.

delme_to_nodes_unsafe_test(_Config) ->
    ok = test_helper:clear_db_for_test(),
    ok = mnesia:start(),
    {atomic,ok} = mnesia:create_table(nodes, [{type, bag}]),
    ok = mnesia:dirty_write({nodes, test_space, node()}),

    ?assert(db_manager:is_node_in_space(node(), test_space)),
    ?assertMatch({atomic, ok}, db_manager:delme_to_nodes_unsafe(test_space)),
    ?assertNot(db_manager:is_node_in_space(node(), test_space)),
    {atomic,ok} = mnesia:delete_table(nodes),
    ?assertMatch({aborted, _}, db_manager:delme_to_nodes_unsafe(test_space)),
    
    ok.