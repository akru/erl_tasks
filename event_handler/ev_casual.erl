-module(ev_casual).
-export([add_listener/2, fire_event/2, start/0]).

start() -> spawn(fun() -> listener([]) end).
fire_event(Pid, Data) -> Pid ! {fire, Data}. 
add_listener(Pid, Mod) -> Pid ! {add, Mod}.

listener(L) ->
    receive
        {add, M} -> listener([M|L]);
        {fire, D} -> lists:map(fun(M) -> M:handle_event(D) end, L),
                     listener(L)
    end.
