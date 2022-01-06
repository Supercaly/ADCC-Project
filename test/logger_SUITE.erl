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
    do_handle_event_test/1,
    format_test/1
]).

all() -> [{group, publics}, {group, internals}].

groups() -> [
    {publics, [shuffle], [logi_test, logw_test, loge_test, log_test]},
    {internals, [shuffle], [do_handle_event_test, format_test]}
].

init_per_group(publics, Config) ->
    gen_event:start({local, logger_event_manager}),
    gen_event:add_handler(logger_event_manager, logger, []),
    Config;
init_per_group(_, Config) -> Config.

end_per_group(public, _) ->
    gen_event:stop(logger_event_manager),
    ok;
end_per_group(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%
% Public functions
%%%%%%%%%%%%%%%%%%%%

logi_test(_Config) -> 
    logger:logi("info message"),
    logger:logi(["list", 1, 2, {a, b}]),
    logger:logi(<<10, 20>>),
    logger:logi(self()),
    ok.
logw_test(_Config) -> 
    logger:logw("warning message"),
    logger:logw(["list", 1, 2, {a, b}]),
    logger:logw(<<10, 20>>),
    logger:logw(self()),
    ok.
loge_test(_Config) ->
    logger:loge("error message"),
    logger:loge(["list", 1, 2, {a, b}]),
    logger:loge(<<10, 20>>),
    logger:loge(self()),
    ok.
log_test(_Config) ->
    logger:log(unknown, "log message"),
    logger:log(unknown, ["list", 1, 2, {a, b}]),
    logger:log(unknown, <<10, 20>>),
    logger:log(unknown, self()),
    ok.

%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%

do_handle_event_test(_Config) -> 
    % log simple messages succed
    logger:do_handle_event({info, "Test msg"}),
    logger:do_handle_event({warning, "Test msg"}),
    logger:do_handle_event({error, "Test msg"}),
    logger:do_handle_event({unknown, "Test msg"}),
    % wrong parameter fails
    ?assertError(function_clause, logger:do_handle_event({})),
    ?assertError(function_clause, logger:do_handle_event([1,2,3])),
    ok.

format_test(_Config) ->
    ["I", 58, 32, "\"test msg\"", "\n"] = logger:format(info, "test msg"),
    ok.