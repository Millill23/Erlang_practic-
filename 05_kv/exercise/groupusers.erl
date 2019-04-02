-module(groupusers).

-export([init/0,generic_functions/0, group_by_gender/1,generic_group_by/2]).


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

generic_functions() ->
  {
  fun(User) ->
    {user,_,Age,_,_,_} = User,
      if
         (Age < 25) andalso (Age >= 0) -> young;
         (Age < 45) andalso (Age >= 0) -> middle;
         (Age > 45) andalso (Age >= 0) -> old;
         true -> whoareyou
      end
    end,

  fun(User) ->
    {user,_,_,Gender,_,FavoriteAnimal} = User,
      if
        (Gender == male) andalso (FavoriteAnimal == cats) -> manWhoLikeCats;
        (Gender == male) andalso (FavoriteAnimal == dogs) -> manWhoLikeDogs;
        (Gender == female) andalso (FavoriteAnimal == cats) -> womanWhoLikeCats;
        (Gender == female) andalso (FavoriteAnimal == dogs) -> womanWhoLikeDogs
      end
    end
  }.

group_by_gender(Users) ->
  [Male,Female] = group_by_gender(Users,[[],[]]),
  #{
    male => Male,
    female => Female
  }.


group_by_gender([],Acc) -> Acc;
group_by_gender([Head | Tail],[Male,Female]) ->
  case Head of
    {user,_,_,male,_,_} -> group_by_gender(Tail,[[Head|Male],Female]);
    {user,_,_,female,_,_} -> group_by_gender(Tail,[Male,[Head|Female]])
  end.

generic_group_by(GF,Users) -> generic_group_by(GF,Users,#{}).

generic_group_by(_,[],Acc) -> Acc;
generic_group_by(GF,[User|Tail],Acc) ->
  case maps:find(GF(User),Acc) of
    error -> generic_group_by(GF,Tail,Acc#{GF(User) => [User]});
    {ok,GroupUsers} -> generic_group_by(GF,Tail,Acc#{GF(User) := [User | GroupUsers]})
  end.
