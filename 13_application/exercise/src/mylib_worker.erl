-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link()->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

get_version()->
  {ok,Vsn} = application:get_key(mylib,vsn),
  Vsn.

get_modules()->
  {ok,Modules} = application:get_key(mylib,modules),
  Modules.

get_min_val()->
  {ok,MinVal} = application:get_env(mylib,min_val),
  MinVal.

get_connection_timeout()->
  {ok,Val} = application:get_env(mylib,connection_timeout),
  Val.

all_apps()->
  List = application:which_applications(),
  F = fun({Name,Descr,Vsn},Acc) -> Acc#{Name => #{description => Descr,
                                                  version => Vsn}} end,
  lists:foldl(F,#{},List).

init([])->
  {ok,#{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
