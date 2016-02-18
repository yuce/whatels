%%%-------------------------------------------------------------------
%% @doc whatels top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(whatels_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ServiceSpec = #{id => whatels_service,
                    start => {whatels_service, start_link, []},
                    restart => permanent,
                    shutdown => 1000,
                    modules => [whatels_service]},
    SupSpec = {{one_for_all, 10, 60}, [ServiceSpec]},
    {ok, SupSpec}.
