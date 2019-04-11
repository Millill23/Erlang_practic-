-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE, []).


init([]) ->
    SupervisorSpecification = #{strategy => one_for_all,
                                intensity => 1,
                                period => 1000},
    Worker3 =  #{id => worker_3,
                 start => {worker, start_link, [worker_1]},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [worker]
                },
    Worker4 =  #{id => worker_4,
                 start => {worker, start_link, [worker_2]},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [worker]
                },
{ok, {SupervisorSpecification, [Worker3, Worker4]}}.

add_worker(WorkerId) ->
    supervisor:start_child(?MODULE, #{id => WorkerId,
                                      start => {worker, start_link, [WorkerId]},
                                      restart => permanent,
                                      shutdown => 2000,
                                      type => worker,
                                      modules => [worker]}).


remove_worker(WorkerId) ->
    supervisor:terminate_child(?MODULE, WorkerId),
    supervisor:delete_child(?MODULE, WorkerId).

  
