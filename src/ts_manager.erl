-module(ts_manager).

-behaviour(gen_server).

-export([
    perform_in/3,
    perform_out/2,
    perform_rd/3,
    start_link/1,
    stop_link/1
]).

% gen_server callbacks.
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

% Export internal functions for testing
-ifdef(TEST).
-export([
    delete_tuple/2,
    match/2,
    read_tuple/2,
    subscribe_for_pattern/3,
    write_tuple/2
]).
-endif.

% Record for the ts_manager state
-record(tsstate, {space}).

%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%

% Perform an out operation on the given tuple space inserting
% the given tuple in it.
% Returns:
%   ok | {error, Reason}
-spec perform_out(Space :: ts:space(), Tuple :: tuple()) -> ts:result().
perform_out(Space, Tuple) when is_atom(Space), is_tuple(Tuple) ->
    case whereis(Space) of
        undefined -> {error, {no_space_with_name, Space}};
        _Pid -> gen_server:call(Space, {write_tuple, Tuple})
    end;
perform_out(Space, Tuple) ->
    {error, {badarg, Space, Tuple}}.

% Perform a rd operation on the given tuple space.
% The operation waits until a tuple matching the given pattern is present or
% after timeout is elapsed. To wait undefinetly pass 'infinity' as timeout.
% Returns:
%   {ok, Tuple} | {error, Reason}
-spec perform_rd(Space :: ts:space(), 
    Pattern :: tuple(), 
    Timeout :: timeout()) -> ts:t_result(tuple()).
perform_rd(Space, Pattern, Timeout) when 
    is_atom(Space), 
    is_tuple(Pattern), 
    (Timeout =:= 'infinity') orelse (is_number(Timeout) andalso Timeout >= 0) ->
    case whereis(Space) of
        undefined -> {error, {no_space_with_name, Space}};
        _Pid -> 
            Res = gen_server:call(Space, {read_tuple, Pattern}),
            case Res of
                {error, no_tuples} -> subscribe_for_pattern(Space, Pattern, Timeout);
                Result -> Result
            end
    end;
perform_rd(Space, Pattern, Timeout) ->
    {error, {badarg, Space, Pattern, Timeout}}.

% Perform a in operation on the given tuple space (read and deletes a tuple).
% The operation waits until a tuple matching the given pattern is present or
% after timeout is elapsed. To wait undefinetly pass 'infinity' as timeout.
% Returns:
%   {ok, Tuple} | {error, Reason}
-spec perform_in(Space :: ts:space(), 
    Pattern :: tuple(), 
    Timeout :: timeout()) -> ts:t_result(tuple()).
perform_in(Space, Pattern, Timeout) when 
    is_atom(Space), 
    is_tuple(Pattern), 
    (Timeout =:= 'infinity') orelse (is_number(Timeout) andalso Timeout >= 0) ->
    case whereis(Space) of
        undefined -> {error, {no_space_with_name, Space}};
        _Pid -> 
            ReadResult = case gen_server:call(Space, {read_tuple, Pattern}) of
                {error, no_tuples} -> subscribe_for_pattern(Space, Pattern, Timeout);
                Result -> Result
            end,
            case ReadResult of
                {ok, Tuple} -> 
                    case gen_server:call(Space, {delete_tuple, Tuple}) of
                        {error, no_tuples} -> perform_in(Space, Tuple, Timeout);
                        ok -> {ok, Tuple};
                        Error -> Error
                    end;
                Error -> Error
            end
    end;
perform_in(Space, Pattern, Timeout) ->
    {error, {badarg, Space, Pattern, Timeout}}.

% Start an instance of ts_manager for the tuple space
% with given name.
-spec start_link(SpaceName :: atom()) -> term().
start_link(SpaceName) when is_atom(SpaceName) ->
    gen_server:start_link({local, SpaceName}, ?MODULE, SpaceName, []).

% Stop the running ts_manager for the tuple space 
% with given name.
-spec stop_link(Space :: ts:space()) -> stopped.
stop_link(Space) when is_atom(Space) -> 
    case whereis(Space) of
        undefined -> {error, {no_space_with_name, Space}};
        _Pid -> gen_server:call(Space, stop)
    end.

%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%

% init/1 callback from gen_server.
init(SpaceName) ->
    process_flag(trap_exit, true),
    logger:logi("ts_manager: initialized", SpaceName),
    {ok, #tsstate{space = SpaceName}}.

% handle_call/3 callback from gen_server.
% Receive messages:
%   {read_tuple, Pattern}   |
%   {write_tuple, Tuple}    |
%   {delete_tuple, Tuple}   |
%   stop
%
% Responds with:
%   ok                  |
%   {ok, Tuple}         |
%   {error, no_tuples}  |
%   {error, Reason}     |
%   stopped
handle_call({read_tuple, Pattern}, _From, State) ->
    Res = read_tuple(State#tsstate.space, Pattern),
    {reply, Res, State};
handle_call({write_tuple, Tuple}, _From, State) ->
    Res = write_tuple(State#tsstate.space, Tuple),
    {reply, Res, State};
handle_call({delete_tuple, Tuple}, _From, State) ->
    Res = delete_tuple(State#tsstate.space, Tuple),
    {reply, Res, State};
handle_call(stop, _From, _State) ->
    {stop, normal, stopped, _State};
handle_call(_Request, _From, _State) ->
    {reply, {error, bad_request}, _State}.

% handle_cast/2 callback from gen_server.
handle_cast(_Msg, _State) ->
    logger:logi("ts_manager: cast message received", _Msg),
    {noreply, _State}.

% handle_info/2 callback from gen_server.
handle_info(_Info, _State) ->
    logger:logi("ts_manager: info message received", _Info),
    {noreply, _State}.

% terminate/2 callback from gen_server.
terminate(_Reason, State) ->
    logger:logi("ts_manager: terminated", State),
    ok.

% code_change/3 callback from gen_server.
code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%

% Read a tuple matching given pattern.
% Returns:
%   {ok, Tuple} | {error, Reson}
-spec read_tuple(Space :: ts:space(), Pattern :: tuple()) -> ts:t_result(tuple()).
read_tuple(Space, Pattern) when is_atom(Space), is_tuple(Pattern) -> 
    case mnesia:transaction(fun() ->
        lists:filter(fun(T) -> 
            match(Pattern, element(3,T)) 
        end, mnesia:read({Space, tuple_size(Pattern)}))
    end) of
        {atomic, []} -> {error, no_tuples};
        {atomic, [T|_]} -> {ok, element(3,T)};
        {aborted, Reason} -> {error, Reason}
    end.

% Insert the given tuple in the tuple space.
% Returns:
% ok | {error, Reason}
-spec write_tuple(Space :: ts:space(), Tuple :: tuple()) -> ts:result().
write_tuple(_, {}) -> ok;
write_tuple(Space, Tuple) when is_atom(Space), is_tuple(Tuple) -> 
    case mnesia:transaction(fun() ->
        mnesia:write({Space, tuple_size(Tuple), Tuple})
    end) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

% Remove the given tuple from the tuple space.
% Returns:
%   ok | {error, Reason}
-spec delete_tuple(Space :: ts:space(), Tuple :: tuple()) -> ts:result().
delete_tuple(_, {}) -> ok;
delete_tuple(Space, Tuple) when is_atom(Space), is_tuple(Tuple) -> 
    case mnesia:transaction(fun() ->
        % read the tuple in the db
        ReadResult = lists:filter(fun(T) -> match(Tuple, element(3,T)) end, mnesia:read({Space, tuple_size(Tuple)})),
        % if the tuple is present remove it otherwise abort the transaction and keep waiting,
        % someone else has deleted it before
        case ReadResult of
            [] -> mnesia:abort({no_tuples});
            [_] -> mnesia:delete_object({Space, tuple_size(Tuple), Tuple})
        end        
    end) of
        {atomic, _} -> ok;
        {aborted, {no_tuples}} -> {error, no_tuples};
        {aborted, Reason} -> {error, Reason}
    end.

% This function must be called after a 'write_tuple' call to ts_manager returns with {error, no_tuples},
% this function listen to all the tuples inserted in the space from now on and matches all of them
% with the given pattern; if a tuple matches or a timeout is reached this function stops listening to new
% tuples and returns the mathed.
% If timeout is set to 'infinity' or no tuple is matching this function wait indefinitly.
% Returns:
%   {ok, Tuple} | {error, timeout}
-spec subscribe_for_pattern(Space :: ts:space(), Pattern :: tuple(), Timeout :: timeout()) -> ts:t_result(tuple()).
subscribe_for_pattern(Space, Pattern, Timeout) when
    is_atom(Space), 
    is_tuple(Pattern), 
    (Timeout =:= 'infinity') orelse (is_number(Timeout) andalso Timeout >= 0) ->
    mnesia:subscribe({table, Space, simple}),
    receive
        {mnesia_table_event, {write, {Space, Size, NewTuple}, _AId}} when Size =:= tuple_size(Pattern) ->
            mnesia:unsubscribe({table, Space, simple}),
            PtrnMatches = match(Pattern, NewTuple),
            if
                PtrnMatches -> {ok, NewTuple};
                true -> subscribe_for_pattern(Space, Pattern, Timeout)
            end
    after
        Timeout ->
            mnesia:unsubscribe({table, Space, simple}),
            {error, timeout}
    end.

% Given a pattern and a tuple verify that the tuple matches
% the pattern. the pattern and the tuple are both tuples, but for matching
% are converted info lists, then every element if checked recursively until
% one differs or every element equals.
% Returns:
%   true | false
-spec match(Pattern :: tuple()|list(), Tuple :: tuple()|list()) -> boolean().
match(Pattern, Tuple) when is_tuple(Pattern), is_tuple(Tuple) -> 
    match(tuple_to_list(Pattern), tuple_to_list(Tuple));
match([], []) -> true;
match(Pattern, Tuple) when is_list(Pattern), is_list(Tuple) ->
    if
        (length(Pattern) == 0) orelse (length(Tuple) == 0) ->
            false;
        true ->
            [PatternHd | PatternTl] = Pattern,
            [TupleHd | TupleTl] = Tuple,
            ElementMatch = match_element(PatternHd, TupleHd),
            if
                ElementMatch -> match(PatternTl, TupleTl);
                true -> false
            end
    end.

% Given two elements, E1 an element from a pattern, E2 an element from
% a tuple, verify that the two elements match.
% If E1 is the atom 'any' the function returns true without checking
% E2, if E1 and E2 are both tuples/lists/maps their elements are checked
% recursively, otherwise it's checked the simple equality.
% Returns:
%   true | false
-spec match_element(E1 :: term(), E2 :: term()) -> boolean().
match_element(E1, _) when E1 == any -> true;
match_element(E1, E2) when is_tuple(E1), is_tuple(E2) -> match(E1, E2);
match_element(E1, E2) when is_list(E1), is_list(E2) -> match(E1, E2);
match_element(E1, E2) when is_map(E1), is_map(E2) -> match(maps:to_list(E1), maps:to_list(E2));
match_element(E1, E2) -> E1 == E2.