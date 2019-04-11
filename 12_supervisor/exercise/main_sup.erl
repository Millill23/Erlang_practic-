-module(main_sup).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE, []).


init([]) ->
    SupervisorSpecification = #{strategy => one_for_one,
                                intensity => 1,
                                period => 1000},
    Sup1 =  #{id => sup_1,
                 start => {sup_1, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => supervisor,
                 modules => [sup_1]
                },
    Sup2 =  #{id => sup_2,
                 start => {sup_2, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => supervisor,
                 modules => [sup_2]
                },
{ok, {SupervisorSpecification, [Sup1, Sup2]}}.
