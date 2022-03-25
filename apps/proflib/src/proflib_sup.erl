-module(proflib_sup).

-behaviour(supervisor).

-export([start_link/0]).

% supervisor callbaks.
-export([init/1]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Start a new instance of proflib_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%
% supervisor callbaks
%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from supervisor.
init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5},

    ChildSpecs = [
        #{
            id => proflib_log,
            start => {proflib_log, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [proflib_log]
        },
        #{
            id => proflib_event_sup,
            start => {proflib_event_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [proflib_event_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
