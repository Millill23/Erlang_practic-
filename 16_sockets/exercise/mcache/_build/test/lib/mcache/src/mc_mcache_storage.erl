-module(mc_mcache_storage).
-behavior(gen_server).

-export([start_link/0,set/2,get/1,gets/1,delete/1,add/2,replace/2,append/2,prepend/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, no_args, []).


set(Key,Val)->
  gen_server:call(?MODULE,{set,Key,Val}).
get(Key)->
  gen_server:call(?MODULE,{get,Key}).
gets(Keys)->
  gen_server:call(?MODULE,{gets,Keys}).
delete(Key)->
  gen_server:call(?MODULE,{delete,Key}).
add(Key,Val)->
  gen_server:call(?MODULE,{add,Key,Val}).
replace(Key,Val)->
  gen_server:call(?MODULE,{replace,Key,Val}).
append(Key,Val)->
  gen_server:call(?MODULE,{append,Key,Val}).
prepend(Key,Val)->
  gen_server:call(?MODULE,{prepend,Key,Val}).
%%% gen_server API

init(no_args) ->
    ets:new(?MODULE,[named_table]),
    State = #state{},
    {ok, State}.

handle_call({get,Key}, _From, State) ->
  case ets:lookup(?MODULE,Key) of
                  [{_,V}] -> Val = <<"VALUE ",V/binary>>;
                  [] -> Val = <<"NOT FOUND">>
  end,
    {reply,<<Val/binary,"\n">>,State};

handle_call({set,Key,Val}, _From, State) ->
    ets:insert(?MODULE,{Key,Val}),
    {reply,<<"STORED\n">>, State};

handle_call({gets,Keys}, _From, State) ->
    F = fun(Key) -> case ets:lookup(?MODULE,Key) of
                    [{_,Val}] -> {Key,Val};
                    [] -> {Key,<<"NOT FOUND">>}
                    end end,
    Vals = lists:map(F,Keys),
    {reply,<<Vals/binary,"\n">>, State};

handle_call({delete,Key}, _From, State) ->
  case ets:take(?MODULE,Key) of
                  [_] -> Rep = <<"DELETED">>;
                  [] -> Rep = <<"NOT FOUND">>
  end,
    {reply,<<Rep/binary,"\n">>,State};

handle_call({add,Key,Val}, _From, State) ->
  case ets:lookup(?MODULE,Key) of
                  [_] -> Rep = <<"EXISTS">>;
                  [] -> Rep = <<"STORED">>,
                        ets:insert(?MODULE,{Key,Val})
  end,
    {reply,<<Rep/binary,"\n">>,State};

handle_call({replace,Key,Val}, _From, State) ->
  case ets:lookup(?MODULE,Key) of
                  [_] -> Rep = <<"STORED">>,
                  ets:insert(?MODULE,{Key,Val});
                  [] -> Rep = <<"NOT FOUND">>

  end,
    {reply,<<Rep/binary,"\n">>,State};

handle_call({append,Key,Val}, _From, State) ->
  case ets:lookup(?MODULE,Key) of
                  [{_,OldVal}] -> Rep = <<"STORED">>,
                  ets:insert(?MODULE,{Key,<<OldVal/binary,Val/binary>>});
                  [] -> Rep = <<"NOT FOUND">>
  end,
    {reply,<<Rep/binary,"\n">>,State};

handle_call({prepend,Key,Val}, _From, State) ->
  case ets:lookup(?MODULE,Key) of
                  [{_,OldVal}] -> Rep = <<"STORED">>,
                  ets:insert(?MODULE,{Key,<<Val/binary,OldVal/binary>>});
                  [] -> Rep = <<"NOT FOUND">>
  end,
    {reply,<<Rep/binary,"\n">>,State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
{ok, State}.
