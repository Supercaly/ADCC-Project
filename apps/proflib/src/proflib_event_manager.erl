-module(proflib_event_manager).

-behaviour(gen_server).

-export([
    start_link/1
]).

% gen_server callbacks.
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

start_link(Event) when is_atom(Event) ->
    gen_server:start_link({local, Event}, ?MODULE, Event, []).

%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_server.
init(Event) ->
    process_flag(trap_exit, true),
    StartTime = erlang:monotonic_time(microsecond),
    {ok, {Event, StartTime}}.

% handle_call/2 callback from gen_server.
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

% handle_cast/2 callback from gen_server.
handle_cast(_Msg, _State) ->
    {noreply, _State}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) ->
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Reason, {Event, StartTime}) ->
    StopTime = erlang:monotonic_time(microsecond),
    proflib_log:log_event(Event, StartTime, StopTime),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.
