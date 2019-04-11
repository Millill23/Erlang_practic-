-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1,close_room/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(user() :: {name(),pid()}).
-type(message() :: {name(),binary()}).

-record(state, {
          users = #{} :: map(),
          messages = [] :: [message()]
}).

start_link()->
  gen_server:start_link(?MODULE,[],[]).

-spec add_user(pid(),name(),pid())-> ok.
add_user(RoomPid,UserName,UserPid) ->
  gen_server:cast(RoomPid,{add_user,UserName,UserPid}),
  ok.

-spec remove_user(pid(),pid()) -> ok | {error, user_not_found}.
remove_user(RoomPid,UserPid) ->
  gen_server:call(RoomPid,{remove_user,UserPid}).

-spec get_users(pid()) -> [user()].
get_users(RoomPid)->
  gen_server:call(RoomPid,get_users).

-spec add_message(pid(),name(),binary()) -> ok.
add_message(RoomPid,UserName,Message) ->
  gen_server:cast(RoomPid,{add_message,UserName,Message}),
  ok.

-spec get_history(pid()) -> [message()].
get_history(RoomPid)->
  gen_server:call(RoomPid,get_history).

-spec close_room(pid()) -> ok.
close_room(RoomPid)->
  gen_server:cast(RoomPid,close_room), ok.

init([])->
  {ok,#state{}}.

handle_call({remove_user,UserPid},_From,#state{users = Users} = State)->
  case maps:find(UserPid,Users) of
    {ok,_UserName} -> {reply,ok,State#state{users = maps:remove(UserPid,Users)}};
    error -> {reply,{error, user_not_found},State}
  end;

handle_call(get_users,_From,#state{users = Users} = State)->
  Reply = lists:reverse(maps:fold(fun(Pid,Name,Acc) -> [{Name,Pid}|Acc] end,[],Users)),
  {reply,Reply,State};

handle_call(get_history,_From,#state{messages = Messages} = State)->
  {reply,lists:reverse(Messages),State}.

handle_cast({add_user,UserName,UserPid},#state{users = Users} = State)->
  {noreply, State#state{users = Users#{UserPid => UserName}}};

handle_cast(close_room,State)->
  exit(normal),
  {noreply, State};

handle_cast({add_message,UserName,Message},#state{messages = Messages, users = Users} = State) ->
  maps:map(fun(PidUser,_NameUser) -> chat_user:add_message(PidUser,UserName,Message), _NameUser end,Users),
  {noreply,State#state{messages = [{UserName,Message} | Messages]}}.


handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
