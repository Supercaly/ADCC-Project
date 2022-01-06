-module(ts_old).

-export([in/2, init/1, match/2, new/1, out/2, rd/2]).

% Node management
% addNode(TS, Node) -> ok.

% removeNode(TS, Node) -> ok.

% nodes(TS) -> ok.

% Tuple Space management
new(Name) ->
    register(me, self()),
    io:format("Create new tuple space \"~p\"\n", [Name]),
    register(Name, spawn(?MODULE, init, [[]])),
    ets:new(dataRam, [bag,named_table]),
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
      % Message for rd operation
      {ts_rd, Pid, Pattern} ->
        case lists:filter(fun(Tuple)-> match(Pattern, Tuple) end, Data) of
          [] -> init(Data);
          [Element | _] ->
            Pid!{ok, Element},
            init(Data)
        end;
      % Message for out operation
      {ts_out, Tuple} ->
        NewData = Data ++ [Tuple],
	      {me,uno@localhost} ! {"Got out for tuple", Tuple},
	      init(NewData);
      % Unknown message
      _Msg -> 
        {me,uno@localhost} ! {unknown, _Msg}, 
        init(Data)
    end.

in(TS, Pattern) -> 
  TS!{ts_in, self(), Pattern},
  receive
    {ok, Tuple} -> Tuple
  end.

% in(TS, Pattern, Timeout) -> ok.

rd(TS, Pattern) ->
  TS!{ts_rd, self(), Pattern},
  receive
    {ok, Tuple} -> Tuple
  end.

% rd(TS, Pattern, Timeout) -> ok.

out(TS, Tuple) -> TS ! {ts_out, Tuple}.

match(L1, L2) -> L1 == L2.

%tuple2list(Tuple) -> [].

