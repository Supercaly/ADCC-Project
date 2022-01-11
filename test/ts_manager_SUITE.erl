-module(ts_manager_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    end_per_group/2,
    groups/0,
    init_per_group/2
]).

-export([
    delete_tuple_test/1,
    perform_in_test/1,
    perform_rd_test/1,
    perform_out_test/1,
    read_tuple_test/1,
    write_tuple_test/1,
    match_test/1
]).

% Dummy record used in match_test.
-record(testrec, {a,b,c}).

all() -> [{group, publics}, {group, internals}].

groups() -> [
    {publics, [shuffle], [perform_out_test, perform_rd_test, perform_in_test]},
    {internals, [shuffle], [match_test, 
        read_tuple_test, write_tuple_test, delete_tuple_test]}
].

init_per_group(publics, Config) -> 
    gen_server:start({local, test_space}, ts_manager, test_space, []),
    Config;
init_per_group(_, Config) -> Config.

end_per_group(publics, _) -> 
    gen_server:stop(test_space),
    ok;
end_per_group(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%
% Public API test cases
%%%%%%%%%%%%%%%%%%%%%%%

perform_out_test(_Config) ->
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),

    ?assertMatch(ok, ts_manager:perform_out(test_space, {a,b})),
    [{_,_,T}|_] = mnesia:dirty_read(test_space, 2),
    ?assertEqual({a,b}, T),
    ?assertMatch({error, {no_space_with_name, test_space2}}, ts_manager:perform_out(test_space2, {a,b})),

    ok.

perform_in_test(_Config) -> 
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),
    mnesia:dirty_write({test_space, 2, {a,b}}),

    ?assertMatch({ok, {a,b}}, ts_manager:perform_in(test_space, {a,b}, 'infinity')),
    ?assertMatch([], mnesia:dirty_read(test_space, 2)),
    {Time, Res} = timer:tc(ts_manager, perform_in, [test_space, {a,b}, 1000]),
    ?assertEqual({error, timeout}, Res),
    ?assert((Time >= 1000000) andalso (Time < 1005000)),
    ?assertMatch({error, {no_space_with_name, test_space2}}, ts_manager:perform_in(test_space2, {a,b}, 200)),
    
    ok.

perform_rd_test(_Config) -> 
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),
    mnesia:dirty_write({test_space, 2, {a,b}}),

    ?assertMatch({ok, {a,b}}, ts_manager:perform_rd(test_space, {a,b}, 'infinity')),
    {Time, Res} = timer:tc(ts_manager, perform_rd, [test_space, {c,d}, 1000]),
    ?assertEqual({error, timeout}, Res),
    ?assert((Time >= 1000000) andalso (Time < 1005000)),
    ?assertMatch({error, {no_space_with_name, test_space2}}, ts_manager:perform_rd(test_space2, {a,b}, 200)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%
% Internal test cases
%%%%%%%%%%%%%%%%%%%%%

read_tuple_test(_Config) ->
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),
    mnesia:dirty_write({test_space, 3, {a,b,c}}),
    mnesia:dirty_write({test_space, 2, {c,d}}),
    mnesia:dirty_write({test_space, 2, {c,a}}),

    % Read specific data
    ?assertMatch({ok, {a,b,c}}, ts_manager:read_tuple(test_space, {a,b,c})),
    ?assertMatch({ok, {c,d}}, ts_manager:read_tuple(test_space, {c,d})),
    ?assertMatch({ok, {c,a}}, ts_manager:read_tuple(test_space, {c,a})),
    % Read data with pattern
    Res = ts_manager:read_tuple(test_space, {c,any}),
    ?assert((Res == {ok,{c,a}}) orelse (Res == {ok,{c,d}})),
    % Read non-existing data
    ?assertMatch({error, no_tuples}, ts_manager:read_tuple(test_space, {a,c,d})),
    ?assertMatch({error, no_tuples}, ts_manager:read_tuple(test_space, {c})),
    ?assertMatch({error, no_tuples}, ts_manager:read_tuple(test_space, {any})),
    ?assertMatch({error, no_tuples}, ts_manager:read_tuple(test_space, {a,any,d})),

    ok.

write_tuple_test(_Config) ->
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),

    ?assertMatch(ok, ts_manager:write_tuple(test_space, {a,b,c})),
    [{_,_,T}|_] = mnesia:dirty_read(test_space, 3),
    ?assertEqual({a,b,c}, T),
    ?assertMatch(ok, ts_manager:write_tuple(test_space, {})),
    ?assertMatch({error, _}, ts_manager:write_tuple(test_space2, {a})),

    ok.

delete_tuple_test(_Config) ->
    test_helper:clear_db_for_test(),
    mnesia:start(),
    mnesia:create_table(test_space, [{type, bag}]),
    mnesia:dirty_write({test_space, 2, {a,b}}),

    ?assertMatch(ok, ts_manager:delete_tuple(test_space, {a,b})),
    ?assertMatch([], mnesia:dirty_read(test_space, 2)),
    ?assertMatch(ok, ts_manager:delete_tuple(test_space, {})),
    ?assertMatch({error, _}, ts_manager:delete_tuple(test_space2, {a})),

    ok.

match_test(_Config) ->
    % basic matching
    ?assertEqual(true,  ts_manager:match({}, {})),
    ?assertEqual(true,  ts_manager:match({a,b}, {a,b})),
    ?assertEqual(false, ts_manager:match({a,b}, {a,b,c})),
    ?assertEqual(false, ts_manager:match({}, {a,b,c})),
    ?assertEqual(false, ts_manager:match({a,b}, {})),
   
    % basic matching with wildcard
    ?assertEqual(true, ts_manager:match({any}, {a})),
    ?assertEqual(true, ts_manager:match({a, any}, {a,b})),
    ?assertEqual(true, ts_manager:match({any, any}, {a,b})),
    
    % match all data types with wildcard
    ?assertEqual(true, ts_manager:match({any}, {1})),
    ?assertEqual(true, ts_manager:match({any}, {1.2})),
    ?assertEqual(true, ts_manager:match({any}, {atom})),
    ?assertEqual(true, ts_manager:match({any}, {<<0,0,0,0,0,0,0,1>>})),
    ?assertEqual(true, ts_manager:match({any}, {make_ref()})),
    ?assertEqual(true, ts_manager:match({any}, {fun(X) -> X+1 end})),
    ?assertEqual(true, ts_manager:match({any}, {self()})),
    ?assertEqual(true, ts_manager:match({{any,any,any}}, {{1,2,3}})),
    ?assertEqual(true, ts_manager:match({#{a=>any,b=>any,c=>any}}, {#{a => 1,b => 2, c => 3}})),
    ?assertEqual(true, ts_manager:match({[any,any,any]}, {[1,2,3]})),
    ?assertEqual(true, ts_manager:match({any}, {[]})),
    ?assertEqual(true, ts_manager:match({any}, {"string"})),
    ?assertEqual(true, ts_manager:match({any}, {true})),
    ?assertEqual(true, ts_manager:match({any}, {false})),
    ?assertEqual(true, ts_manager:match({{testrec,any,any,any}}, {#testrec{a=1,b=2,c=3}})),
    ?assertEqual(true, ts_manager:match({#testrec{a=any,b=any,c=any}}, {#testrec{a=1,b=2,c=3}})),
    
    % match some particular case
    ?assertEqual(true, ts_manager:match({any}, {{1,2,3}})),
    ?assertEqual(false, ts_manager:match({[any,any,any]}, {{1,2,3}})),
    ?assertEqual(false, ts_manager:match({[]}, {{1,2,3}})),
    ?assertEqual(false, ts_manager:match({[{a,any},{b,any},{c,any}]}, {#{a => 1,b => 2, c => 3}})),
    ?assertEqual(false, ts_manager:match({[any]}, {[1,2,3]})),
    ?assertEqual(true, ts_manager:match({[]}, {[]})),
    
    % match nested elements
    ?assertEqual(true, ts_manager:match({{1,{2,{3}}},[1,[[2,[3]]]]}, {{1,{2,{3}}},[1,[[2,[3]]]]})),
    ?assertEqual(false, ts_manager:match({{1,{2,{3}}},[1,[[2,[4]]]]}, {{1,{2,{3}}},[1,[[2,[3]]]]})),
    
    ok.