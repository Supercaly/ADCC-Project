-module(ts_app).

-behaviour(application).

% application callbacks.
-export([
    start/2,
    stop/1
]).

% start/2 callback from application.
start(normal, _Args) -> 
    main_sup:start_link();
start(_, _) -> 
    {error, badarg}.

% stop/1 callback from application.
stop(_State) -> 
    ok.