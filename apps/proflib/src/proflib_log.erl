-module(proflib_log).

-behaviour(gen_server).

-export([
    start_link/0,
    log_event/3
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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_event(Event, StartTime, StopTime) ->
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:cast(?MODULE, {new_event, Event, StartTime, StopTime})
    end.

%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_server.
init(_Args) ->
    process_flag(trap_exit, true),
    FilePath = case application:get_env(out_path) of
        undefined -> "./";
        {ok, P} -> P
    end,
    FileName = atom_to_list(node()) ++ "-log.txt",
    FullPath = FilePath ++ FileName,
    ok = filelib:ensure_dir(FullPath),
    {ok, Fd} = file:open(FullPath, [write]),
    {ok, [Fd]}.

% handle_call/2 callback from gen_server.
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

% handle_cast/2 callback from gen_server.
handle_cast({new_event, Event, StartTime, StopTime}, [File]) ->
    Msg = io_lib:format("~p ~p ~p ~p~n", [Event, 
        StartTime, 
        StopTime,
        (StopTime - StartTime)]),
    ok = file:write(File, Msg),
    {noreply, [File]}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) ->
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Reason, [File]) ->
    ok = file:close(File),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.
