-module(casino).

-compile([export_all]).

start(Count) when Count > 1 ->
    % Spawn processes
    Poll = spawnN(Count, fun() -> member_proc(0, []) end),

    % Send member list to any process
    [P ! {members, Poll} || P <- Poll],

    % Start lotery
    [P ! {start} || P <- Poll],

    ok.

spawnN(N, X) when N > 0 -> [spawn(X) | spawnN(N - 1, X)];
spawnN(_, _) -> [].

member_proc(Result, Members) ->
    receive
        {members, NewMembers} ->
            io:format("New member list for ~p is ~p~n", [self(), NewMembers]),
            random:seed(erlang:phash2([node()]),
                        erlang:monotonic_time(),
                        erlang:unique_integer()),
            NewResult = random:uniform(100), 
            io:format("Lotery result of ~p is ~p~n", [self(), NewResult]),
            member_proc(NewResult, NewMembers);

        {start} ->
            [P ! {result, Result, self()} || P <- Members],
            member_proc(Result, Members);

        {result, N, Pid} when N > Result ->
            io:format("The ~p received great result ~p from ~p, gone =(~n",
                      [self(), N, Pid]);

        _ -> member_proc(Result, Members)
    end.
