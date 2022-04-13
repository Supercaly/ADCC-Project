-module(proflib).

-export([
    start/0,
    start/1,
    stop/0,
    begine/1,
    ende/1
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

begine(Event) when is_atom(Event) -> 
    proflib_event_sup:start_event(Event).

ende(Event) when is_atom(Event) -> 
    proflib_event_sup:stop_event(Event).