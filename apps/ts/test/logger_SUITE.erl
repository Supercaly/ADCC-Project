-module(logger_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2
]).

-export([
    logi_test/1,
    logw_test/1,
    loge_test/1,
    log_test/1,
    do_handle_log_test/1,
    level_string_test/1
]).

all() -> [{group, publics}, {group, internals}].

groups() -> [
    {publics, [shuffle], [logi_test, logw_test, loge_test, log_test]},
    {internals, [shuffle], [do_handle_log_test, level_string_test]}
].

init_per_group(publics, Config) ->
    gen_server:start({local, logger}, logger, [], []),
    Config;
init_per_group(_, Config) -> Config.

end_per_group(public, _) ->
    gen_server:stop(logger),
    ok;
end_per_group(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%
% Public functions
%%%%%%%%%%%%%%%%%%%%

logi_test(_Config) -> 
    logger:logi("info message"),
    logger:logi("info message ", ["list", 1, 2, {a, b}]),
    logger:logi("info message ", <<10, 20>>),
    logger:logi("info message ", self()),
    ok.
logw_test(_Config) -> 
    logger:logw("warning message"),
    logger:logw("warning message ", ["list", 1, 2, {a, b}]),
    logger:logw("warning message ", <<10, 20>>),
    logger:logw("warning message ", self()),
    ok.
loge_test(_Config) ->
    logger:loge("error message"),
    logger:loge("error message ", ["list", 1, 2, {a, b}]),
    logger:loge("error message ", <<10, 20>>),
    logger:loge("error message ", self()),
    ok.
log_test(_Config) ->
    logger:log(unknown, "log message", '_'),
    logger:log(unknown, "log message ", ["list", 1, 2, {a, b}]),
    logger:log(unknown, "log message ", <<10, 20>>),
    logger:log(unknown, "log message ", self()),
    ok.

%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%

do_handle_log_test(_Config) -> 
    % log simple messages succed
    logger:do_handle_log({info, "Test msg", '_'}),
    logger:do_handle_log({warning, "Test msg", '_'}),
    logger:do_handle_log({error, "Test msg", '_'}),
    logger:do_handle_log({unknown, "Test msg", '_'}),
    % wrong parameter fails
    ?assertError(function_clause, logger:do_handle_log({})),
    ?assertError(function_clause, logger:do_handle_log([1,2,3])),
    ok.

level_string_test(_Config) ->
    ?assertEqual("I", logger:level_string(info)),
    ?assertEqual("W", logger:level_string(warning)),
    ?assertEqual("E", logger:level_string(error)),
    ?assertEqual("?", logger:level_string(abc)),
    ok.