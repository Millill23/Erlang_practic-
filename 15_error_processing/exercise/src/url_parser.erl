-module(url_parser).

-export([parse/1]).

-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL)->
  try
    {ok,Protocol,TP} = parse_protocol(URL),
    {ok,Domain,TD} = parse_domain(TP),
    {ok,Query,TQ} = parse_query(TD),
    Path = lists:delete(<<>>,binary:split(TQ,<<"/">>,[global])),
    {ok,Date} = parse_date(Path),
    Res = #{protocol => Protocol,domain => Domain, path => Path, query => Query, date => validate_data(Date)},
    {ok,Res}
  catch
    throw:Error -> Error
  end.


-spec parse_protocol(binary()) -> {ok, binary(),binary()} | {error, invalid_protocol}.
parse_protocol(URL) ->
  case binary:split(URL,<<"://">>) of
      [Protocol,Tail] -> {ok,Protocol,Tail};
      _ -> throw({error,invalid_protocol})
  end.

-spec parse_domain(binary()) -> {ok, binary(),binary()} | {error, invalid_domain}.
parse_domain(URL)->
  case binary:split(URL,<<"/">>) of
      [<<>>] -> throw({error,invalid_domain});
      [Domain] -> {ok,Domain,<<>>};
      [Domain,Tail] -> {ok,Domain,Tail}
  end.

-spec parse_query(binary()) -> {ok, binary(),binary()}.
parse_query(<<>>)-> {ok,<<>>,<<>>};
parse_query(URL)->
  case binary:split(URL,<<"?">>) of
    [Path,Query] -> {ok,Query,Path};
    [Path]-> {ok,<<>>,Path}
  end.

-spec parse_date(list()) -> {ok, {integer(),integer(),integer()}} | {ok,undefined}.
parse_date(Path)->
  case  Path of
    [Y,M,D|_] -> try
                {ok,{binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D)}}
                catch
                  error:badarg -> {ok,undefined}
                end;
    _ -> {ok,undefined}
  end.

-spec validate_data({integer(),integer(),integer()}) -> {ok, {integer(),integer(),integer()}} | {ok,undefined}.
validate_data({Y,M,D}) when D >=1,D =< 31, M >=1, M =< 12 -> {Y,M,D};
validate_data(_) -> undefined.
