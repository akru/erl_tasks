-module(ring).

-compile([export_all]).

start(Count, Round) when Count > 1, Round > 1 ->
    % Spawn childs
    Poll = spawnN(Count, fun() -> ring_proc({}, Round) end),
    % Assing pointers for the ring
    lists:zipwith(fun(P, X) -> P ! {next, X} end, Poll, shift(Poll)),
    % Send marker
    [H|_] = Poll, H ! {marker, self()}.

shift([H|T]) -> T ++ [H]. 

spawnN(N, X) when N > 0 -> [spawn(X) | spawnN(N - 1, X)];
spawnN(_, _) -> [].

ring_proc(Next, Round) when Round > 0 ->
    receive
        {next, NewNext} ->
            io:format("New next for ~p is ~p~n", [self(), NewNext]),
            ring_proc(NewNext, Round);

        {marker, Sender} ->
            Next ! {marker, self()},
            io:format("Marker catch from ~p in ~p~n", [Sender, self()]),
            ring_proc(Next, Round - 1);
        _ ->
            ring_proc(Next, Round)
    end;

ring_proc(_, _) -> io:format("Process ~p exit~n", [self()]).
