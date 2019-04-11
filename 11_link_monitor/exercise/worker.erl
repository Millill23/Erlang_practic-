-module(worker).

-export([run/2]).

run(ReducePid,File)->
  {ok,Binary} = file:read_file(File),
  Items = binary:split(Binary,[<<"\n">>],[global]),
  F =   fun(<<>>,Acc) -> Acc;
           (Item,Acc) -> [_ID,Name,Count,_Price] = binary:split(Item,[<<",">>],[global]),
                         Acc#{Name => list_to_integer(binary_to_list(Count))} end,
  Result = lists:foldl(F,#{},Items),
  ReducePid ! {result,Result}.
