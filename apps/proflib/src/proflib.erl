-module(proflib).

-export([
    start/0,
    start/1,
    stop/0,
    begin_event/1,
    end_event/1,
    begin_task/2,
    end_task/2
]).

start(Context) ->
    application:load(proflib_app),
    ok = application:set_env(proflib_app, out_path, Context),
    start().

start() ->
    case application:start(proflib_app) of
        ok -> ok;
        {error, {already_started, proflib_app}} -> ok;
        Other -> Other
    end.

stop() ->
    case application:stop(proflib_app) of
        ok -> stopped;
        {error, {not_started, proflib_app}} -> stopped;
        Other -> Other
    end.

begin_event(Event) when is_atom(Event) -> 
    proflib_event_sup:start_event(Event).

end_event(Event) when is_atom(Event) -> 
    proflib_event_sup:stop_event(Event).

begin_task(TaskId, EventName) ->
    begin_event(list_to_atom(lists:flatten(io_lib:format("task_~b_~s",[TaskId,EventName])))).

end_task(TaskId, EventName) ->
    end_event(list_to_atom(lists:flatten(io_lib:format("task_~b_~s",[TaskId,EventName])))).