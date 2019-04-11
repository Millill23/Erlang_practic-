-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(message() :: {name(),binary()}).

start_link()->
  gen_server:start_link(?MODULE,[],[]).

-spec add_message(pid(),name(),binary()) -> ok.
add_message(UserPid,FromName,Message)->
  gen_server:cast(UserPid,{add_message,FromName,Message}),
  ok.

-spec get_messages(pid()) -> [message()].
get_messages(UserPid)->
  gen_server:call(UserPid,get_messages).


init([]) ->
  {ok,[]}.

handle_call(get_messages,_From,State)->
    {reply, lists:reverse(State), State}.

handle_cast({add_message,FromName,Message},State)->
    {noreply, [{FromName,Message} | State]}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
