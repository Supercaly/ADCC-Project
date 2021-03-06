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
-spec add_space_manager(SpaceName :: atom()) -> ts:result().
add_space_manager(SpaceName) when is_atom(SpaceName) ->
    case supervisor:start_child(?MODULE, [SpaceName]) of
        {ok, _Child} -> ok;
        {error, Reason} -> {error, {cant_start_tsmanager, Reason}}
    end.

% Stop the instance of ts_manager for given space.
-spec del_space_manager(SpaceName :: atom()) -> ts:result().
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
            shutdown => 2000
        }
    ],

    {ok, {Flags, ChildSpecs}}.