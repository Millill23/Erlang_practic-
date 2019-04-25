-module(mc_mcache_server).
-behavior(gen_server).

-export([start_server/0,start_acceptor/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).




start_server()->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_acceptor(ID,ListenSocket)->
  {ok,AcceptSocket} = gen_tcp:accept(ListenSocket),
  handle_connect(ID,ListenSocket,AcceptSocket).

init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, NumAcceptors} = application:get_env(mcache, numAcceptors),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE,start_acceptor,[ID,ListenSocket]) || ID <- lists:seq(1,NumAcceptors)],
{ok, ListenSocket}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


handle_connect(ID,ListenSocket,AcceptSocket)->
  case gen_tcp:recv(AcceptSocket,0) of
    {ok,Msg} ->
            Answer = parse_msg(Msg),
            gen_tcp:send(AcceptSocket,<<Answer/binary, "\r\n">>),
            handle_connect(ID,ListenSocket,AcceptSocket);
    {error,closed}->
      start_acceptor(ID,ListenSocket)
    end.

parse_msg(Msg1)->
  Msg = binary:replace(Msg1,<<"\r\n">>,<<>>),
  case binary:split(Msg,<<" ">>) of
  [Command,Tail] ->
        case Command of
          <<"SET">> -> [Key,Val] = binary:split(Tail,<<" ">>),
                        mc_mcache_storage:add(Key,Val);

          <<"GET">> -> Key = Tail,
                        A = mc_mcache_storage:get(Key),
                        <<A/binary>>;

          <<"GETS">> -> Keys = binary:split(Tail,<<" ">>,[global]),
                        Answer = mc_mcache_storage:gets(Keys),
                        F = fun({Key,Val},Acc) -> <<Acc/binary,"VALUE ",Key/binary," ",Val/binary,"\r\n">> end,
                        A = lists:foldl(F,<<>>,Answer),
                        <<A/binary,"END">>;

          <<"DELETE">> -> Key = Tail,
                        mc_mcache_storage:delete(Key);

          <<"ADD">> ->  [Key,Val] = binary:split(Tail,<<" ">>),
                        mc_mcache_storage:add(Key,Val);

          <<"REPLACE">> -> [Key,Val] = binary:split(Tail,<<" ">>),
                        mc_mcache_storage:replace(Key,Val);

          <<"APPEND">> -> [Key,Val] = binary:split(Tail,<<" ">>),
                        mc_mcache_storage:append(Key,Val);

          <<"PREPEND">> -> [Key,Val] = binary:split(Tail,<<" ">>),
                        mc_mcache_storage:prepend(Key,Val);

          _ -> <<"UNKNOWN REQUEST">>
        end;
  [_] -> <<"UNKNOWN REQUEST">>
end.
