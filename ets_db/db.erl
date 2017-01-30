-module(db).
-behaviour(gen_server).
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

-export([new/0,
         write/3,
         delete/2,
         read/1,
         match/1,
         stat/0,
         avg/0
        ]).

code_change(_, S, _) -> {ok, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.

init(_) ->
    {ok, ets:new(kv_db, [protected, set, named_table])}.

handle_call({set, Key, Val}, _, Tab) ->
    try
        ets:insert(Tab, {Key, Val}),
        {reply, ok, Tab}
    catch
        E -> {reply, {error, E}, Tab}
    end;

handle_call({delete, Key}, _, Tab) ->
    try
        ets:delete(Tab, Key),
        {reply, ok, Tab}
    catch
        E -> {reply, {error, E}, Tab}
    end.

terminate(_, _) -> ok.

new() ->
    {ok, Db} = gen_server:start(db, {}, []),
    Db.

write(Serv, Key, Val) ->
    gen_server:call(Serv, {set, Key, Val}).

delete(Serv, Key) ->
    gen_server:call(Serv, {delete, Key}).

read(Key) ->
    {ok, ets:lookup(kv_db, Key)}.

match(Val) ->
    {ok, lists:concat(ets:match(kv_db, {'$1', Val}))}.

stat() ->
    {size, N} = lists:keyfind(size, 1, ets:info(kv_db)),
    {ok, N}.

avg() ->
    {ok, N} = stat(),
    Avg = fun({_, V}, A) when is_number(V) -> A + V / N; (_, A) -> A end,
    {ok, ets:foldl(Avg, 0, kv_db)}.
