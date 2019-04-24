%%%-------------------------------------------------------------------
%% @doc mcache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mcache_sup).

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

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupervisorSpecification = #{
      strategy => one_for_one,
      intensity => 10,
      period => 60},

  ChildSpecifications =
      [#{id => mc_mcache_server,
         start => {mc_mcache_server, start_server, []},
         restart => permanent,
         shutdown => 2000,
         type => worker,
         modules => [mc_mcache_server]},
       #{id => mc_mcache_storage,
          start => {mc_mcache_storage, start_link, []},
          restart => permanent,
          shutdown => 2000,
            type => worker,
            modules => [mc_mcache_storage]}
      ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.


%%====================================================================
%% Internal functions
%%====================================================================
