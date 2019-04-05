-module(map_reduce).

-export([start/1,maps_reduce/2,mapping/2, file_to_map/1,glue_maps/1]).


start(Files) ->
  MainPid = self(),
  spawn(?MODULE,maps_reduce,[Files,MainPid]),
  receive
    {reduce,MainMap} -> MainMap
  end.

maps_reduce(Files,MainPid)->
  MapsReducePid = self(),
  [spawn(?MODULE,mapping,[File,MapsReducePid]) || File <-Files],
  AllMaps = maps_reduce(length(Files),0,[]),
  MainMap = glue_maps(AllMaps),
  MainPid ! {reduce,MainMap}.

maps_reduce(StopC,Counter,Acc) when StopC==Counter -> Acc;
maps_reduce(StopC,Counter,Acc)->
  receive
    {map,Txt} -> maps_reduce(StopC,Counter+1,[Txt|Acc])
  end.

mapping(File,MapsReducePid) ->
  Txt = file_to_binary(File),
  MapsReducePid ! {map,Txt}.


file_to_binary(File)->
  case file:read_file(File) of
    {ok,Txt} -> txt_to_map(Txt);
    {error,_} -> #{}
  end.

txt_to_map(Txt)->
  Words = binary:split(Txt,[<<" ">>,<<"\n">>],[global]),
  txt_to_map(Words,#{}).

txt_to_map([],Acc) -> Acc;
txt_to_map([Word|Tail],Acc) ->
  case maps:find(Word,Acc) of
    {ok,Nums} -> txt_to_map(Tail,Acc#{ Word := Nums+1});
    error -> txt_to_map(Tail,Acc#{Word => 1})
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
