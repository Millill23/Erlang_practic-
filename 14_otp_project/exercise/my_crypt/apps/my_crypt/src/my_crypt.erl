-module(my_crypt).
-behavior(gen_server).

-export([start_link/0,encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

encode(Data)->
  gen_server:call(?MODULE,{encode,Data}).

get_key()->
  gen_server:call(?MODULE,get_key).

set_key(NewKey)->
  gen_server:cast(?MODULE,{set_key,NewKey}).

hash(Data)->
  List = binary_to_list(Data),
  Hashtable = gen_server:call(?MODULE,get_hashtable),
  hash(List,Hashtable).

hash(Data,Hashtable)->
  {ok,HashSize} = application:get_env(my_crypt,hash_size),
  F1 = fun(Byte,Hash) -> lists:nth(Byte bxor Hash+1,Hashtable) end,
  F2 = fun(Number)-> lists:foldl(F1,Number,Data) end,
  list_to_binary(lists:map(F2,lists:seq(1,HashSize))).


init([]) ->
  {ok, Key} = application:get_env(my_crypt, encode_key),
  {ok,{A,B,C}} = application:get_env(my_crypt,seed),
  rand:seed(exsp, {A,B,C}),
  Hashtable = create_hash_table(),
  State = #{key => Key, hashtable => Hashtable},
  %io:format("worker start at ~p with state ~p ~n",[self(),State]),
  {ok,State}.

handle_call({encode,BinaryData}, _From, State) ->
    Data = binary_to_list(BinaryData),
    {ok,BKey} = maps:find(key,State),
    Key = binary_to_list(real_key(BKey,BinaryData,BKey)),
    Res = list_to_binary(encode(Data,Key,[])),
    {reply, Res, State};

handle_call(get_key, _From, State) ->
    {ok,Key} = maps:find(key,State),
    {reply, Key, State};

handle_call(get_hashtable, _From, State) ->
    {ok,Table} = maps:find(hashtable,State),
    {reply, Table, State}.

handle_cast({set_key,NewKey}, State) ->
    {noreply, State#{key := NewKey}}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
{ok, State}.

real_key(Key,Data,Acc)->
  case byte_size(Acc) >= byte_size(Data) of
    true -> Acc;
    false -> real_key(Key,Data,<<Key/binary,Acc/binary>>)
  end.

encode([],_Key,Acc) -> lists:reverse(Acc);
encode([FirstByteData|TailData],[FirstByteKey|TailKey],Acc)->
  encode(TailData,TailKey,[FirstByteData bxor FirstByteKey|Acc]).

create_hash_table()->
  Range = lists:seq(0, 255),
  shuffle(Range, []).

shuffle([], Res) -> Res;
shuffle(List, Res) ->
  RandomByte = rand:uniform(256) -1,
  shuffle(lists:delete(RandomByte, List), [RandomByte | Res]).
