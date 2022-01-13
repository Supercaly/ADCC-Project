-module(logger).

-behaviour(gen_server).

-export([
    log/3,
    loge/1,
    loge/2,
    logi/1,
    logi/2,
    logw/1,
    logw/2,
    start_link/0
]).

% gen_server callbaks.
-export([
    code_change/3, 
    handle_call/3, 
    handle_cast/2,
	handle_info/2, 
    init/1, 
    terminate/2
]).

% Export internal functions for testing
-ifdef(TEST).
-export([do_handle_log/1, level_string/1]).
-endif.

%%%%%%%%%%%%%%
% Custom types
%%%%%%%%%%%%%%

-type log_level() :: info | warning | error.
-export_type([log_level/0]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Log a given info message to the terminal.
-spec logi(Message :: string()) -> ok.
logi(Message) -> log(info, Message, '_').

% Log a given info message to the terminal.
-spec logi(Message :: string(), Args :: term()) -> ok.
logi(Message, Args) -> log(info, Message, Args).

% Log a given warning message to the terminal.
-spec logw(Message :: string()) -> ok.
logw(Message) -> log(warning, Message, '_').

% Log a given warning message to the terminal.
-spec logw(Message :: string(), Args :: term()) -> ok.
logw(Message, Args) -> log(warning, Message, Args).

% Log a given error message to the terminal.
-spec loge(Message :: string()) -> ok.
loge(Message) -> log(error, Message, '_').

% Log a given error message to the terminal.
-spec loge(Message :: string(), Args :: term()) -> ok.
loge(Message, Args) -> log(error, Message, Args).

% Log given message to the terminal with given log level.
-spec log(Level :: log_level(), Message :: string(), Args :: term()) -> ok.
log(Level, Message, Args) ->
    gen_server:cast(?MODULE, {Level, Message, Args}),
    ok.

% Start an instance of LOGGER_EVENT_MANAGER.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%
% gen_server callbaks
%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_server.
init(_Args) -> 
    process_flag(trap_exit, true),
    do_handle_log({info, "logger: initialized", '_'}),
    {ok, []}.

% handle_call/2 callback from gen_server.
handle_call(_Request, _From, _State) -> 
    do_handle_log({warning, "logger: call", _Request}),
    {reply, ok, _State}.

% handle_cast/2 callback from gen_server.
handle_cast(Msg, _State) -> 
    do_handle_log(Msg),
    {noreply, _State}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) -> 
    do_handle_log({info, "logger: info", _Info}),
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Args, _State) -> 
    do_handle_log({info, "logger: terminated", '_'}),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) -> 
    {ok, _State}.

% Pretty prints the given log event.
do_handle_log({Level, Msg, '_'}) ->
    io:put_chars(standard_io, 
        io_lib:format("~s: ~s~n", [level_string(Level), Msg]));
do_handle_log({Level, Msg, Args}) ->
    io:put_chars(standard_io, 
        io_lib:format("~s: ~s ~p~n", [level_string(Level), Msg, Args])).

% Return the string representation of the log level.
level_string(Level) ->
    case Level of
        info -> "I";
        warning -> "W";
        error -> "E";
        _ -> "?"
    end.
