-module(kvdb_server).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         read/2,
         write/3,
         delete/2,
         match/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read(Srv, Key) ->
    gen_server:call(Srv, {read, Key}).

write(Srv, Key, Val) ->
    gen_server:call(Srv, {write, Key, Val}).

delete(Srv, Key) ->
    gen_server:call(Srv, {delete, Key}).

match(Srv, Val) ->
    gen_server:call(Srv, {match, Val}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, []}.

handle_call(Request, _From, State) ->
    {Reply, NewState} =
        case Request of
            {read, Key}       -> {read_db(State, Key), State};
            {match, Val}      -> {match_db(State, Val), State};
            {write, Key, Val} -> write_db(State, Key, Val);
            {delete, Key}     -> delete_db(State, Key)
    end,
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_db(_, []) -> {error, 404}; 
read_db(K, [H|T]) ->
    case H of
        {K, V} -> {ok, V};
        _      -> read_db(K, T)
    end.

delete_db(K, D) -> delete_(K, D, []).
delete_(_, [], A)    -> {ok, A};
delete_(K, [H|T], D) ->
    case H of
        {K, _} -> delete_(K, T, D);
        _      -> delete_(K, T, [H|D])
    end.

write_db(K, E, D) ->
    case read_db(K, D) of
        {error, _} -> {ok, [{K, E} | D]};
        _          -> write_db(K, E, delete_db(K, D))
    end.

match_db(V, D) -> match_(V, D, []).
match_(_, [], A)    -> {ok, A};
match_(V, [H|T], A) ->
    case H of
        {K, V} -> match_(V, T, [K|A]);
        _ -> match_(V, T, A)
    end.
