-module(test_helper).

-export([
    clear_db_for_test/0
]).

%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%

clear_db_for_test() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok.