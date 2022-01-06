-module(ts_manager).

-behaviour(gen_server).

-export([
    start_link/0,
    stop_link/0
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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_link() ->
    gen_server:call(?MODULE, stop).

init(_Args) ->
    logger:logi("ts_manager: initialized"),
    {ok, []}.

handle_call(stop, _From, _State) ->
    {stop, normal, stopped, _State};
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info(_Info, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    logger:logi("ts_manager: terminated"),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.
