-module(graph).

-compile([export_all]).

start(Count) when Count > 1 ->
    % Spawn processes
    [H|T] = spawnN(Count, fun() -> graph_proc([]) end),
    % Assing pointers for the graph
    make_graph([H], [[]], T),
    % Send marker
    H ! {marker, self()},
    H.

spawnN(N, X) when N > 0 -> [spawn(X) | spawnN(N - 1, X)];
spawnN(_, _) -> [].

make_graph(_, _, []) -> ok;

make_graph([], [H|T], Poll) -> make_graph(H, T, Poll);

make_graph([H|_], _, Poll) when length(Poll) < 5 -> H ! {childs, Poll};

make_graph([H|T], Childs, Poll) ->
    {C, X} = lists:split(random:uniform(5), Poll),
    H ! {childs, C},
    make_graph(T, Childs ++ [C], X).

graph_proc(Childs) ->
    receive
        {childs, NewChilds} ->
            io:format("New childs for ~p is ~p~n", [self(), NewChilds]),
            graph_proc(NewChilds);

        {marker, Sender} ->
            io:format("Marker catch from ~p in ~p~n", [Sender, self()]),
            [Next ! {marker, self()} || Next <- Childs],
            graph_proc(Childs);
        _ ->
            graph_proc(Childs)
    end.
