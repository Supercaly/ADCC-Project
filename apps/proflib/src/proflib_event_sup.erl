-module(proflib_event_sup).

-behaviour(supervisor).

-export([
    start_event/1,
    stop_event/1,
    start_link/0
]).

%% supervisor callbacks.
-export([init/1]).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Start a new instance of proflib_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_event(Event) when is_atom(Event) ->
    case whereis(Event) of
        undefined -> ok;
        _Pid -> stop_event(Event)
    end,
    case supervisor:start_child(?MODULE, [Event]) of
        {ok, _Child} -> ok;
        {error, Reason} -> {error, {cant_start_tsmanager, Reason}}
    end.

stop_event(Event) when is_atom(Event) ->
    supervisor:terminate_child(?MODULE, whereis(Event)).

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
            id => proflib_event_manager,
            start => {proflib_event_manager, start_link, []},
            shutdown => 2000
        }
    ],
    
    {ok, {Flags, ChildSpecs}}.