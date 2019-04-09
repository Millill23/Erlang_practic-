-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4, get_messages_history/2,stop/1,loop/1]).

-record(room, {
             id = 0,
             name = roomName,
             users = [],
             msgHistory = []
  }).

start()->
  State = #{room_limit => 5, rooms => #{}},
  spawn(?MODULE,loop,[State]).

create_room(Server, RoomName) ->
    call(Server,create_room,RoomName).

remove_room(Server, RoomId) ->
    call(Server,remove_room,RoomId).

get_rooms(Server) ->
    call(Server,get_rooms,ok).

add_user(Server, RoomId, UserName) ->
    call(Server,add_user,{RoomId,UserName}).

remove_user(Server, RoomId, UserName) ->
    call(Server,remove_user,{RoomId,UserName}).

get_users_list(Server, RoomId) ->
    call(Server,get_users_list,RoomId).

send_message(Server, RoomId, UserName, Message) ->
    call(Server,send_message,{RoomId,UserName,Message}).

get_messages_history(Server, RoomId) ->
    call(Server,get_messages_history,RoomId).

call(ServerPid,Command,Payload)->
  Ref = erlang:monitor(process,ServerPid),
  ServerPid ! {Command,Ref,self(),Payload},
  receive
    {reply,Ref,Reply} -> erlang:demonitor(Ref,[flush]),
                        Reply;
    {'DOWN',Ref,process,ServerPid,Reason} ->
        io:format("Server crashed with Reason: ~p~n",[Reason]),
        {error,Reason}
  after
    4000 -> erlang:demonitor(Ref,[flush]),
            {error,no_reply}
  end.


stop(Pid) ->
  Pid ! stop.


loop(State)->
  io:format("Chat manager V 0.1! loop ~p with state ~p~n",[self(),State]),
    receive
      {Command,Ref,ClientPid,Payload} ->
                    {Reply,State2} = handle_call(Command,Payload,State),
                    ClientPid ! {reply,Ref,Reply},
                    ?MODULE:loop(State2);
      stop ->
                    io:format("GS stoped from user~n"), ok;
      Msg ->
                    io:format("loop got ~p~n",[Msg]),?MODULE:loop(State)
    end.




handle_call(create_room,RoomName,State) ->
          {ok,Rooms} = maps:find(rooms,State),
          CurrSize = maps:size(Rooms),
          case maps:find(room_limit,State) of
            {ok,Limit} when Limit > CurrSize ->
                        RoomId = make_ref(),
                        NewRooms = Rooms#{RoomId =>#room{id = RoomId, name = RoomName}},
                        State2 = State#{rooms := NewRooms},
                        {{ok,RoomId},State2};
            _ -> {{error, room_limit},State}
          end;

handle_call(remove_room,RoomId,State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,_Room} ->
                      NewRooms = maps:remove(RoomId,Rooms),
                      State2 = State#{rooms := NewRooms},
                      {ok,State2};
          error -> {{error, room_not_found},State}
        end;

handle_call(add_user,{RoomId,UserName},State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,{room,_,_,Users,_}=Room} ->
                                  Fun = fun(User) -> User == UserName end,
                                  case lists:any(Fun,Users) of
                                    true -> {{error, user_is_in_room},State};
                                    false -> NewRoom = Room#room{users = [UserName | Users]},
                                            {ok,State#{rooms := Rooms#{RoomId := NewRoom}}}
                                          end;
          error -> {{error, room_not_found},State}
        end;

handle_call(remove_user,{RoomId,UserName},State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,{room,_,_,Users,_}=Room} ->
                                  Fun = fun(User) -> User == UserName end,
                                  case lists:any(Fun,Users) of
                                    true -> NewRoom = Room#room{users = lists:delete(UserName,Users)},
                                            {ok,State#{rooms => Rooms#{RoomId := NewRoom}}};
                                    false ->
                                            {{error, user_not_in_room},State}
                                          end;
          error -> {{error, room_not_found},State}
        end;

handle_call(get_users_list,RoomId,State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,{room,_,_,Users,_}} -> {{ok,Users},State};
          error -> {{error, room_not_found},State}
        end;

handle_call(send_message,{RoomId,UserName,Message},State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,{room,_,_,Users,MessageHistory}=Room} ->
                                  Fun = fun(User) -> User == UserName end,
                                  case lists:any(Fun,Users) of
                                    true -> {ok,State#{rooms => Rooms#{RoomId := Room#room{msgHistory = [{UserName,Message}|MessageHistory]}}}};
                                    false -> {{error, user_not_in_room},State}
                                          end;
          error -> {{error, room_not_found},State}
        end;

handle_call(get_messages_history,RoomId,State) ->
        {ok,Rooms} = maps:find(rooms,State),
        case maps:find(RoomId,Rooms) of
          {ok,{room,_,_,_,MessageHistory}} -> {{ok,MessageHistory},State};
          error -> {{error, room_not_found},State}
        end;


 handle_call(get_rooms,ok,State) ->
              {ok,Rooms} = maps:find(rooms,State),
              F = fun(Key,{room,_,Name,_,_},Acc) -> [{Key,Name}| Acc] end,
              {maps:fold(F,[],Rooms),State}.
