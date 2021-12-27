-module(ts).

-export([init/1, new/1, out/2, match/2]).

% Node management
% addNode(TS, Node) -> ok.

% removeNode(TS, Node) -> ok.

% nodes(TS) -> ok.

% Tuple Space management
new(Name) ->
    register(me, self()),
    io:format("Create new tuple space \"~p\"\n", [Name]),
    register(Name, spawn(?MODULE, init, [[]])),
    {ok}.

init(Data) ->
    me ! {all_data, Data},
    receive
      % Message for in operation
      {ts_in, Pid, Pattern} -> 
        case lists:filter(fun (Tuple)-> match(Pattern, Tuple) end, Data) of
          [] -> init(Data);
          [Element | _] -> 
            Pid!{ok, Element},
            NewData = lists:delete(Element, Data),
            init(NewData)
        end;
      % Message for out operation
      {ts_out, Tuple} ->
	      NewData = Data ++ [Tuple],
	      me ! {"Got out for tuple", Tuple},
	      init(NewData);
      % Unknown message
      _Msg -> 
        me ! {unknown, _Msg}, 
        init(Data)
    end.

% in(TS, Pattern) -> ok.

% in(TS, Pattern, Timeout) -> ok.

% rd(TS, Pattern) -> ok.

% rd(TS, Pattern, Timeout) -> ok.

out(TS, Tuple) -> TS ! {ts_out, Tuple}.

match(L1, L2) -> L1 == L2.

%tuple2list(Tuple) -> [].

