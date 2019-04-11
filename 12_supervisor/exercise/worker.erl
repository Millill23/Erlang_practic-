-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(id() :: term()).

start_link(ID)->
  gen_server:start_link(?MODULE,[ID],[]).

-spec ping(pid()) -> {id(),pid()}.
ping(Pid)->
  gen_server:call(Pid,ping).

init(ID)->
  {ok,#{id => ID}}.

handle_call(ping,_From,State)->
  {reply,{maps:get(id,State),self()},State}.

handle_cast(_, State)->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
