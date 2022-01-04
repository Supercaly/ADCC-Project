-module(main_sup).

-behaviour(supervisor).

-export([start_link/0]).

% supervisor callbaks.
-export([init/1]).

% Start a new instance of main_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% init/0 callback from supervisor.
init(_Args) ->
    % TODO: Tune the ration between intensity and period
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5},

    ChildSpecs = [
        #{
            id => logger,
            start => {logger, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => dynamic
        },
        #{
            id => db_manager,
            start => {db_manager, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [db_manager]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
