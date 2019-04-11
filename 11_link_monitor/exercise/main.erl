-module(main).

-export([parse/1,reducer/2]).


parse(Files) ->
    MainPid = self(),
    spawn(?MODULE,reducer,[Files,MainPid]),
    receive
      {reduce,MainTuple} -> MainTuple
    after
      4000 -> {error,no_reply}
    end.

reducer(Files,MainPid)->
    Workers = lists:map(
              fun(File)->
                          Pid = spawn(worker,run,[self(),File]),
                          Ref = erlang:monitor(process, Pid),
                          {Pid,Ref,File} end,Files),
    {ResultMaps,ErrorMap} = waiter(Workers,{[],#{}}),
    Result = {glue_maps(ResultMaps),ErrorMap},
    MainPid ! {reduce,Result}.

  waiter([],Acc) -> Acc;
  waiter([{Pid,Ref,File}|Tail] = Workers,{ResMaps,ErrorMap}=Acc)->
    receive
      {result,Map} -> waiter(Workers,{[Map|ResMaps],ErrorMap});
      {'DOWN', Ref, process, Pid, Reason} ->
                              case Reason of
                                normal -> waiter(Tail,Acc);
                                _ -> waiter(Tail,{ResMaps,ErrorMap#{File => Reason}})
                              end

    end.

glue_maps(AllMaps)->
  glue_maps(AllMaps,#{}).

glue_maps([],Acc)-> Acc;
glue_maps([Map|Tail],Acc)->
  F = fun(Word,Counter,NewMap)-> case maps:find(Word,NewMap)of
                                    {ok,Counter1} -> NewMap#{Word := Counter+Counter1};
                                    error -> NewMap#{Word => Counter}
                                  end
                                end,
  NewAcc = maps:fold(F,Acc,Map),
glue_maps(Tail,NewAcc).
