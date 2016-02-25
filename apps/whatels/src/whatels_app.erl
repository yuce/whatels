-module(whatels_app).
-behaviour(application).

-export([start/2,
         stop/1]).

%% == API

start(_StartType, _StartArgs) ->
    case os:getenv("WHATELS_PORT") of
        false ->
            case application:get_env(whatels, port) of
                undefined ->
                    whatels_sup:start_link();
                {ok, Port} ->
                    whatels_sup:start_link([{port, Port}])
            end;
        StrPort ->
            Port = list_to_integer(StrPort),
            whatels_sup:start_link([{port, Port}])
    end.

stop(_State) ->
    ok.
