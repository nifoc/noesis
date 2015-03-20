% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc The `noesis_string' module provides functions for working with strings.

-module(noesis_string).

% Types

-type hexstring() :: string().

-export_type([
  hexstring/0
]).

% API
-export([
  levenshtein/2,
  hamming/2
]).

% API

% @doc Calculates the Levenshtein distance between two strings.<br /><br />
%      Partially based on a <a href="http://rosettacode.org/wiki/Levenshtein_distance#Erlang" target="_blank">Rosetta Code</a>
%      example.
-spec levenshtein(string(), string()) -> non_neg_integer().
levenshtein(First, Second) ->
  {Distance, _Dict} = levenshtein_rec(First, Second, dict:new()),
  Distance.

% @doc Calculates the Hamming distance between two strings of equal length.
-spec hamming(string(), string()) -> non_neg_integer().
hamming(First, Second) when length(First) =:= length(Second) ->
  hamming_acc(First, Second, 0).

% Private

-spec levenshtein_rec(string(), string(), noesis_dict:dictionary(tuple(), non_neg_integer())) -> {non_neg_integer(), noesis_dict:dictionary(tuple(), non_neg_integer())}.
levenshtein_rec(Str, Str, Dict) ->
  {0, Dict};
levenshtein_rec([_FH], [_SH], Dict) ->
  {1, Dict};
levenshtein_rec([], Second, Dict) ->
  {length(Second), Dict};
levenshtein_rec(First, [], Dict) ->
  {length(First), Dict};
levenshtein_rec([FH|FT]=First, [SH|ST]=Second, Dict) ->
  case dict:is_key({First, Second}, Dict) of
    true ->
      Length = dict:fetch({First, Second}, Dict),
      {Length, Dict};
    false ->
      {Distance1, Dict2} = levenshtein_rec(First, ST, Dict),
      {Distance2, Dict3} = levenshtein_rec(FT, Second, Dict2),
      {Distance3A, Dict4} = levenshtein_rec(FT, ST, Dict3),
      {Distance3B, Dict5} = levenshtein_rec([FH], [SH], Dict4),
      Length = lists:min([Distance1 + 1, Distance2 + 1, Distance3A + Distance3B]),
      Dict6 = dict:store({First, Second}, Length, Dict5),
      {Length, Dict6}
  end.

-spec hamming_acc(string(), string(), non_neg_integer()) -> non_neg_integer().
hamming_acc([], [], Acc) ->
  Acc;
hamming_acc([H|FT], [H|ST], Acc) ->
  hamming_acc(FT, ST, Acc);
hamming_acc([_FH|FT], [_SH|ST], Acc) ->
  hamming_acc(FT, ST, Acc + 1).
