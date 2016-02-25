
-module(whatels_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% == API

start_link() ->
    start_link([]).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

%% == Callbacks

init([Args]) ->
    ServiceSpec = #{id => whatels_service,
                    start => {whatels_service, start_link, [Args]},
                    restart => permanent,
                    shutdown => 1000,
                    modules => [whatels_service]},
    SupSpec = {{one_for_all, 10, 60}, [ServiceSpec]},
    {ok, SupSpec}.
