-module(db).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

new() -> spawn(fun() -> db_proc([]) end).
destroy(Pid) -> exit(Pid, exit).

read(Key, Db) ->
    case is_process_alive(Db) of
        true ->
            Db ! {get, Key, self()},
            receive 
                {ok, I} -> {ok, I};
                _ -> {error, bad_response}
            end;
        _ -> {error, db_process_dead}
    end.

write(Key, Val, Db) ->
    case is_process_alive(Db) of
        true ->
            Db ! {set, Key, Val, self()},
            receive
                ok -> ok;
                 _ -> {error, bad_response}
            end;
        _ -> {error, db_process_dead}
    end.

delete(Key, Db) ->
    case is_process_alive(Db) of
        true ->
            Db ! {delete, Key, self()},
            receive
                ok -> ok;
                 _ -> {error, bad_response}
            end;
        _ -> {error, db_process_dead}
    end.

match(Val, Db) ->
    case is_process_alive(Db) of
        true ->
            Db ! {match, Val, self()},
            receive
                {ok, R} -> {ok, R};
                 _ -> {error, bad_response}
            end;
        _ -> {error, db_process_dead}
    end.

db_proc(Db) ->
    NewDb =
      receive 
          {get, Key, Pid} ->
              Pid ! read_db(Key, Db),
              Db;

          {set, Key, Val, Pid} ->
              Pid ! ok,
              write_db(Key, Val, Db);

          {delete, Key, Pid} ->
              Pid ! ok,
              delete_db(Key, Db);

          {match, Val, Pid} ->
              Pid ! match_db(Val, Db),
              Db
      end,
    db_proc(NewDb).

read_db(_, []) -> {error, instance}; 
read_db(K, [H|T]) ->
    case H of
        {K, V} -> {ok, V};
        _      -> read_db(K, T)
    end.

delete_db(K, D) -> delete_(K, D, []).
delete_(_, [], A)    -> A;
delete_(K, [H|T], D) ->
    case H of
        {K, _} -> delete_(K, T, D);
        _      -> delete_(K, T, [H|D])
    end.

write_db(K, E, D) ->
    case read_db(K, D) of
        {error, _} -> [{K, E} | D];
        _          -> write_db(K, E, delete_db(K, D))
    end.

match_db(V, D) -> match_(V, D, []).
match_(_, [], A)    -> {ok, A};
match_(V, [H|T], A) ->
    case H of
        {K, V} -> match_(V, T, [K|A]);
        _ -> match_(V, T, A)
    end.
