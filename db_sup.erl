-module(db_sup).

-behaviour(supervisor).

% public exports
-export([start_link/0]).

% supervisor callbacks
-export([init/1]).

% start_link/0 starts a new local instance of db_sup supervisor.
start_link() -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% init/1 callback from supervisor.
init(_Args) -> 
    Specs = #{strategy => one_for_one},
    Workers = [
        #{
            id => db_monitor,
            start => {db_monitor, start_link, []}
        }
    ],
    {ok, {Specs, Workers}}.