-module(main).

-export([sample_champ/0, get_stat/1, filter_sick_players/1, make_pairs/2]).
-include_lib("eunit/include/eunit.hrl").


sample_champ() ->
    [
     {team, "Crazy Bulls",
      [{player, "Big Bull",        22, 545, 99},
       {player, "Small Bull",      18, 324, 95},
       {player, "Bull Bob",        19,  32, 45},
       {player, "Bill The Bull",   23, 132, 85},
       {player, "Tall Ball Bull",  38,  50, 50},
       {player, "Bull Dog",        35, 201, 91},
       {player, "Bull Tool",       29,  77, 96},
       {player, "Mighty Bull",     22, 145, 98}
      ]},
     {team, "Cool Horses",
      [{player, "Lazy Horse",      21, 423, 80},
       {player, "Sleepy Horse",    23, 101, 35},
       {player, "Horse Doors",     19,  87, 23},
       {player, "Rainbow",         21, 200, 17},
       {player, "HoHoHorse",       20, 182, 44},
       {player, "Pony",            25,  96, 76},
       {player, "Hippo",           17, 111, 96},
       {player, "Hop-Hop",         31, 124, 49}
      ]},
     {team, "Fast Cows",
      [{player, "Flash Cow",       18,  56, 34},
       {player, "Cow Bow",         28,  89, 90},
       {player, "Boom! Cow",       20, 131, 99},
       {player, "Light Speed Cow", 21, 201, 98},
       {player, "Big Horn",        23,  38, 93},
       {player, "Milky",           25,  92, 95},
       {player, "Jumping Cow",     19, 400, 98},
       {player, "Cow Flow",        18, 328, 47}
      ]},
     {team, "Fury Hens",
      [{player, "Ben The Hen",     57, 403, 83},
       {player, "Hen Hen",         20, 301, 56},
       {player, "Son of Hen",      21, 499, 43},
       {player, "Beak",            22,  35, 96},
       {player, "Superhen",        27,  12, 26},
       {player, "Henman",          20,  76, 38},
       {player, "Farm Hen",        18, 131, 47},
       {player, "Henwood",         40, 198, 77}
      ]},
     {team, "Extinct Mosters",
      [{player, "T-Rex",           21, 999, 99},
       {player, "Velociraptor",    29, 656, 99},
       {player, "Giant Mammoth",   30, 382, 99},
       {player, "The Big Croc",    42, 632, 99},
       {player, "Huge Pig",        18, 125, 98},
       {player, "Saber-Tooth",     19, 767, 97},
       {player, "Beer Bear",       24, 241, 99},
       {player, "Pure Horror",     31,  90, 43}
      ]}
    ].


%%-record(stat, {
%%              num_teams = 0,
%%              num_players = 0,
%%              avg_age = 0,
%%              avg_rating = 0
%%              }).

get_stat(Champ) ->
  Champ_total_stat = lists:foldl(
    fun(Team,Acc) ->
      {team,_Name,Players} = Team,
      {NT,NP,AA,AR} = Acc,
      Acc1 = {NT +1, lists:foldl(
                                    fun(_Player, NP1) ->
                                      NP1+1
                                    end,NP,Players),
                     lists:foldl(
                                    fun(Player,AA1) ->
                                      {player,_,Age,_,_} = Player,
                                      AA1+Age
                                    end,AA,Players),
                     lists:foldl(
                                    fun(Player,AR1) ->
                                      {player,_,_,Rating,_} = Player,
                                      AR1 + Rating
                                    end,AR,Players)},
      Acc1
    end,{0,0,0,0},Champ),
  {NT,NP,AA,AR} = Champ_total_stat,
  {NT,NP,AA/NP,AR/NP}.

get_stat_test() ->
    ?assertEqual({5,40,24.85,242.8}, get_stat(sample_champ())),
    ok.


filter_sick_players(Champ) -> filter_sick_players(Champ,[]).

filter_sick_players([],Acc) -> lists:reverse(Acc);
filter_sick_players([Head | Tail],Acc) ->
  {team,NameTeam,PlayersTeam} = Head,
  F = fun({player,_,_,_,CurrHealth}) -> CurrHealth >= 50 end,
  Acc1 = lists:filter(F,PlayersTeam),
  case length(Acc1) >= 5 of
    true -> filter_sick_players(Tail,[{team,NameTeam,Acc1}|Acc]);
    false -> filter_sick_players(Tail,Acc)
  end.


filter_sick_players_test() ->
    Result = [{team, "Crazy Bulls",
               [{player, "Big Bull",        22, 545, 99},
                {player, "Small Bull",      18, 324, 95},
                {player, "Bill The Bull",   23, 132, 85},
                {player, "Tall Ball Bull",  38,  50, 50},
                {player, "Bull Dog",        35, 201, 91},
                {player, "Bull Tool",       29,  77, 96},
                {player, "Mighty Bull",     22, 145, 98}
               ]},
              {team, "Fast Cows",
               [{player, "Cow Bow",         28,  89, 90},
                {player, "Boom! Cow",       20, 131, 99},
                {player, "Light Speed Cow", 21, 201, 98},
                {player, "Big Horn",        23,  38, 93},
                {player, "Milky",           25,  92, 95},
                {player, "Jumping Cow",     19, 400, 98}
               ]},
              {team, "Extinct Mosters",
               [{player, "T-Rex",           21, 999, 99},
                {player, "Velociraptor",    29, 656, 99},
                {player, "Giant Mammoth",   30, 382, 99},
                {player, "The Big Croc",    42, 632, 99},
                {player, "Huge Pig",        18, 125, 98},
                {player, "Saber-Tooth",     19, 767, 97},
                {player, "Beer Bear",       24, 241, 99}
               ]}
             ],
    ?assertEqual(Result, filter_sick_players(sample_champ())),
    ok.


make_pairs(Team1, Team2) ->
  {team, _Name1, Players1} = Team1,
  {team, _Name2, Players2} = Team2,
  [
    {Player1, Player2} || {player, Player1, _Age1, R1, _Health1} <- Players1,
                          {player, Player2, _Age, R2, _Health2} <- Players2,
                          R1 + R2 > 600
  ].


make_pairs_test() ->
    [T1, T2, T3, T4, _] = sample_champ(),
    ?assertEqual([{"Big Bull","Lazy Horse"},
                  {"Big Bull","Sleepy Horse"},
                  {"Big Bull","Horse Doors"},
                  {"Big Bull","Rainbow"},
                  {"Big Bull","HoHoHorse"},
                  {"Big Bull","Pony"},
                  {"Big Bull","Hippo"},
                  {"Big Bull","Hop-Hop"},
                  {"Small Bull","Lazy Horse"},
                  {"Bull Dog","Lazy Horse"}],
                 main:make_pairs(T1, T2)),
    ?assertEqual([{"Lazy Horse","Light Speed Cow"},
                  {"Lazy Horse","Jumping Cow"},
                  {"Lazy Horse","Cow Flow"}],
                 main:make_pairs(T2, T3)),
    ?assertEqual([{"Ben The Hen","Light Speed Cow"},
                  {"Ben The Hen","Jumping Cow"},
                  {"Ben The Hen","Cow Flow"},
                  {"Hen Hen","Jumping Cow"},
                  {"Hen Hen","Cow Flow"},
                  {"Son of Hen","Boom! Cow"},
                  {"Son of Hen","Light Speed Cow"},
                  {"Son of Hen","Jumping Cow"},
                  {"Son of Hen","Cow Flow"}],
                 main:make_pairs(T4, T3)),
ok.
