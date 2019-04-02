-module(groupusers).

-export([init/0,generic_functions/0, group_by_gender/1,generic_group_by/2]).
-include_lib("eunit/include/eunit.hrl").


init() ->
  [
  {user,"Bob",31,male,pizza,cats},
  {user,"Bill",51,male,pasta,cats},
  {user,"Jane",13,female,salad,dogs},
  {user,"Kate",21,female,salad,cats},
  {user,"Jhon",41,male,pizza,dogs},
  {user,"Petya",26,male,pasta,dogs},
  {user,"Alesya",18,female,salad,cats},
  {user,"Masha",19,female,salad,dogs}
  ].

group_by_gender([]) -> #{};
group_by_gender(Users) ->
  [Male,Female] = group_by_gender(Users,[[],[]]),
  #{
    male => lists:reverse(Male),
    female => lists:reverse(Female)
  }.


group_by_gender([],Acc) -> Acc;
group_by_gender([Head | Tail],[Male,Female]) ->
  case Head of
    {user,_,_,male,_,_} -> group_by_gender(Tail,[[Head|Male],Female]);
    {user,_,_,female,_,_} -> group_by_gender(Tail,[Male,[Head|Female]])
  end.

group_by_gender_test() ->
  ?assertEqual(#{male => [
                            {user,"Bob",31,male,pizza,cats},
                            {user,"Bill",51,male,pasta,cats},
                            {user,"Jhon",41,male,pizza,dogs},
                            {user,"Petya",26,male,pasta,dogs}
                            ],
                 female => [
                            {user,"Jane",13,female,salad,dogs},
                            {user,"Kate",21,female,salad,cats},
                            {user,"Alesya",18,female,salad,cats},
                            {user,"Masha",19,female,salad,dogs}
                            ]}, group_by_gender(init())),
  ?assertEqual(#{},group_by_gender([])),
  ok.


generic_group_by(GF,Users) -> generic_group_by(GF,Users,#{}).

generic_group_by(_,[],Acc) -> reverse_value(Acc);
generic_group_by(GF,[User|Tail],Acc) ->
  case maps:find(GF(User),Acc) of
    error -> generic_group_by(GF,Tail,Acc#{GF(User) => [User]});
    {ok,GroupUsers} -> generic_group_by(GF,Tail,Acc#{GF(User) := [User | GroupUsers]})
  end.

reverse_value(Maps) ->
  Keys = maps:keys(Maps),
  reverse_value(Maps,Keys).

reverse_value(Maps,[]) -> Maps;
reverse_value(Maps,[Key|OtherKeys]) ->
  Value = maps:get(Key,Maps),
  reverse_value(Maps#{Key := lists:reverse(Value)},OtherKeys).


generic_group_by_test() ->
  
  F1 = fun(User) ->
    {user,_,Age,_,_,_} = User,
      if
         (Age < 25) andalso (Age >= 0) -> young;
         (Age < 45) andalso (Age >= 0) -> middle;
         (Age > 45) andalso (Age >= 0) -> old;
         true -> whoareyou
      end
    end,
    
  ?assertEqual(#{young => [
                            {user,"Jane",13,female,salad,dogs},
                            {user,"Kate",21,female,salad,cats},
                            {user,"Alesya",18,female,salad,cats},
                            {user,"Masha",19,female,salad,dogs}
                            ],
                 middle => [
                            {user,"Bob",31,male,pizza,cats},
                            {user,"Jhon",41,male,pizza,dogs},
                            {user,"Petya",26,male,pasta,dogs}
                            ],
                 old => [
                            {user,"Bill",51,male,pasta,cats}
                 ]}, generic_group_by(F1,init())),
                 
    F2 = fun(User) ->
     {user,_,_,Gender,_,FavoriteAnimal} = User,
       if
         (Gender == male) andalso (FavoriteAnimal == cats) -> manWhoLikeCats;
         (Gender == male) andalso (FavoriteAnimal == dogs) -> manWhoLikeDogs;
         (Gender == female) andalso (FavoriteAnimal == cats) -> womanWhoLikeCats;
         (Gender == female) andalso (FavoriteAnimal == dogs) -> womanWhoLikeDogs
       end
     end,
     
  ?assertEqual(#{manWhoLikeCats =>
                                  [{user,"Bob",31,male,pizza,cats},
                                  {user,"Bill",51,male,pasta,cats}],
                 manWhoLikeDogs =>
                                  [{user,"Jhon",41,male,pizza,dogs},
                                  {user,"Petya",26,male,pasta,dogs}],
                womanWhoLikeCats =>
                                  [{user,"Kate",21,female,salad,cats},
                                  {user,"Alesya",18,female,salad,cats}],
                womanWhoLikeDogs =>
                                  [{user,"Jane",13,female,salad,dogs},
                                  {user,"Masha",19,female,salad,dogs}]}, generic_group_by(F2,init())),
  ok.
