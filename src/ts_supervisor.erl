-module(ts_supervisor).

-behaviour(supervisor).

-export([
    add_space_manager/1,
    del_space_manager/1,
    start_link/0
]).

%% supervisor callbacks.
-export([init/1]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Start a new instance of ts_supervisor.
-spec start_link() -> term().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Start a new instance of ts_manager for given space.
-spec add_space_manager(SpaceName :: atom()) -> term().
add_space_manager(SpaceName) when is_atom(SpaceName) ->
    supervisor:start_child(?MODULE, [SpaceName]).

% Stop the instance of ts_manager for given space.
-spec del_space_manager(SpaceName :: atom()) -> term().
del_space_manager(SpaceName) when is_atom(SpaceName) ->
    supervisor:terminate_child(?MODULE, whereis(SpaceName)).

%%%%%%%%%%%%%%%%%%%%%
% supervisor callbaks
%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from supervisor.
init(_Args) ->
    Flags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },

    ChildSpecs = [
        #{
            id => ts_manager,
            start => {ts_manager, start_link, []},
            shutdown => brutal_kill
        }
    ],

    {ok, {Flags, ChildSpecs}}.