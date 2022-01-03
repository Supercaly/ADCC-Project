-module(log).

-behaviour(gen_event).

-export([
    start_link/0,
    logi/1,
    logw/1,
    loge/1,
    log/2
]).

% gen_event callbaks.
-export([
    code_change/3, 
    handle_call/2, 
    handle_event/2,
	handle_info/2, 
    init/1, 
    terminate/2
]).

-type log_level() :: info | warning | error.

-export_type([log_level/0]).

-define(LOG_EVENT_MANAGER, log_event_manager).

% Log a given info message to the terminal.
-spec logi(term()) -> ok.
logi(Message) -> log(info, Message).

% Log a given warning message to the terminal.
-spec logw(term()) -> ok.
logw(Message) -> log(warning, Message).

% Log a given error message to the terminal.
-spec loge(term()) -> ok.
loge(Message) -> log(error, Message).

% Log given message to the terminal with given log level.
-spec log(log_level(), term()) -> ok.
log(Level, Message) ->
    gen_event:notify(?LOG_EVENT_MANAGER, {Level, Message}),
    ok.

% Start an instance of log_event_manager.
start_link() ->
    gen_event:start_link({local, ?LOG_EVENT_MANAGER}),
    gen_event:add_handler(?LOG_EVENT_MANAGER, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%
% gen_event callbaks
%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_event.
init(_Args) -> 
    {ok, []}.

% handle_event/2 callback from gen_event.
handle_event(Event, State) -> 
    do_handle_event(Event),
    {ok, State}.

% handle_call/2 callback from gen_event.
handle_call(Request, State) -> 
    do_handle_event({warning, Request}),
    {ok, no_reply, State}.

% handle_info/2 callback from gen_event.
handle_info(Info, State) -> 
    do_handle_event({info, Info}),
    {ok, State}.

% terminate/2 callback from gen_event.
terminate(_Args, _State) -> 
    ok.

% code_change/3 callback from gen_event.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

% Pretty prints the given log event.
do_handle_event({Level, Msg}) ->
    case Level of
        info -> io:format("I: ~p\n", [Msg]);
        warning -> io:format("W: ~p\n", [Msg]);
        error -> io:format("E: ~p\n", [Msg])
    end.