-module(mylib_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartT, _StartA) ->
    mylib_sup:start_link().

stop(_State) ->
ok.

