-module(sup_1).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).


init([]) ->
    SupervisorSpecification = #{strategy => one_for_all,
                                intensity => 1,
                                period => 1000},
    Worker1 =  #{id => worker_1,
                 start => {worker, start_link, [worker_1]},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [worker]
                },
    Worker2 =  #{id => worker_2,
                 start => {worker, start_link, [worker_2]},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [worker]
                },
{ok, {SupervisorSpecification, [Worker1, Worker2]}}.
