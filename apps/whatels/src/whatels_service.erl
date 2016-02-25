-module(whatels_service).
-behaviour(monkey_service).

-export([start_link/0,
         start_link/1]).
-export([init/2, handler_args/1, handle/2]).

-define(DEFAULT_PORT, 10999).

%% == API

start_link() ->
    start_link([{port, ?DEFAULT_PORT}]).

start_link(Args) ->
    monkey_service:start_link(?MODULE, whatels_handler, Args).

%% == Callbacks

init(_Handler, _Args) ->
    {ok, undefined}.

handler_args(_State) ->
    {ok, undefined}.

handle(Msg, State) ->
    io:format("service message received: ~p~n", [Msg]),
    {ok, State}.

