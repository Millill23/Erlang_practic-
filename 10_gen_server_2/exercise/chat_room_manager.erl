-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(user() :: {name(),pid()}).
-type(room() :: {name(),pid()}).
-type(message() :: {name(),binary()}).

start_link()->
  gen_server:start_link({local,manager},?MODULE,[],[]).

-spec create_room(name()) -> {name(),pid()}.
create_room(RoomName)->
  gen_server:call(manager,{create_room,RoomName}).

-spec get_rooms()-> [room()].
get_rooms()->
  gen_server:call(manager,get_rooms).

-spec add_user(pid(),name(),pid()) -> ok | {error, room_not_found}.
add_user(RoomPid,UserName,UserPid)->
  gen_server:call(manager,{add_user,RoomPid,UserName,UserPid}).

-spec remove_user(pid(),pid()) -> ok | {error, user_not_found} | {error, room_not_found}.
remove_user(RoomPid,UserPid)->
  gen_server:call(manager,{remove_user,RoomPid,UserPid}).

-spec get_users(pid()) -> {ok,[user()]} | {error, room_not_found}.
get_users(RoomPid)->
  gen_server:call(manager,{get_users,RoomPid}).

-spec send_message(pid(),name(),binary()) -> ok | {error, room_not_found}.
send_message(RoomPid,FromName,Message)->
  gen_server:call(manager,{send_message,RoomPid,FromName,Message}).

-spec get_history(pid()) -> [message()] | {error, room_not_found}.
get_history(RoomPid)->
  gen_server:call(manager,{get_history,RoomPid}).

init([])->
  {ok,#{}}.

handle_call({create_room,RoomName},_From,State)->
  {ok,RoomPid} = chat_room:start_link(),
  {reply,{RoomName,RoomPid},State#{RoomPid => RoomName}};

handle_call(get_rooms,_From,State)->
  Reply = lists:reverse(maps:fold(fun(Pid,Name,Acc) -> [{Name,Pid}|Acc] end,[],State)),
  {reply,Reply,State};

handle_call({add_user,RoomPid,UserName,UserPid},_From,State)->
  case maps:find(RoomPid,State) of
    {ok,_RoomName} -> {reply,chat_room:add_user(RoomPid,UserName,UserPid),State};
    error -> {reply,{error, room_not_found},State}
  end;

handle_call({remove_user,RoomPid,UserPid},_From,State)->
  case maps:find(RoomPid,State) of
    {ok,_RoomName} -> {reply,chat_room:remove_user(RoomPid,UserPid),State};
    error -> {reply,{error, room_not_found},State}
  end;

handle_call({get_users,RoomPid},_From,State)->
  case maps:find(RoomPid,State) of
    {ok,_RoomName} -> {reply,{ok,chat_room:get_users(RoomPid)},State};
    error -> {reply,{error, room_not_found},State}
  end;

handle_call({send_message,RoomPid,FromName,Message},_From,State)->
  case maps:find(RoomPid,State) of
    {ok,_RoomName} -> {reply,chat_room:add_message(RoomPid,FromName,Message),State};
    error -> {reply,{error, room_not_found},State}
  end;

handle_call({get_history,RoomPid},_From,State)->
  case maps:find(RoomPid,State) of
    {ok,_RoomName} -> {reply,{ok,chat_room:get_history(RoomPid)},State};
    error -> {reply,{error, room_not_found},State}
  end.

handle_cast(_, State)->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
