-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
  Parts = binary:split(Str,[<<"{{">>],[global]),
  parse(Parts,Data,[]).

parse([],_,Acc) -> unicode:characters_to_binary(lists:reverse(Acc));
parse([Part|Tail],Data,Acc)->
  case binary:split(Part,[<<"}}">>]) of
    [Any] -> parse(Tail,Data,[Any|Acc]);
    [NameValue|Any] ->
      case maps:find(NameValue,Data) of
        {ok,Value} when is_binary(Value);is_list(Value) -> parse(Tail,Data,[[Value|Any]|Acc]);
        {ok,Value} when is_integer(Value) -> parse(Tail,Data,[[integer_to_binary(Value)|Any]|Acc]);
        error -> parse(Tail,Data,[Any|Acc])
      end
    end.
