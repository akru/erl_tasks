-module(ev_gen).
-behaviour(gen_event).
-export([code_change/3,
         format_status/2,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2]).

code_change(_, S, _) -> {ok, S}.
format_status(_, _) -> ok.
init(_) -> {ok, {}}.
terminate(_, _) -> ok.
handle_info(_, S) -> {ok, S}.

handle_call(Req, S) ->
    io:format("Is called with ~s~n", [Req]),
    {ok, ok, S}.

handle_event(Eve, S) ->
    io:format("Event ~s catched~n", [Eve]),
    {ok, S}.
