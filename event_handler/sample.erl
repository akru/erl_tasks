-module(sample).

-export([handle_event/1]).

handle_event(E) -> io:format("Event ~s catched!", [E]).
