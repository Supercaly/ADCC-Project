-module(proflib_app).

-behavior(application).

-export([
    start/2,
    stop/1
]).

start(normal, _Args) ->
    proflib_sup:start_link().

stop(_State) ->
    ok.