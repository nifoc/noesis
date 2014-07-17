% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
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

-include("noesis_types.hrl").

% API
-export([
  levenshtein/2
]).

% API

% @doc Calculates the Levenshtein distance between two strings.
-spec levenshtein(string(), string()) -> non_neg_integer().
levenshtein(First, Second) ->
  {Distance, _Dict} = levenshtein_rec(First, Second, dict:new()),
  Distance.

% Private

-spec levenshtein_rec(string(), string(), noesis_dict()) -> {non_neg_integer(), noesis_dict()}.
levenshtein_rec(Str, Str, Dict) ->
  Dict2 = dict:store({Str, Str}, 0, Dict),
  {0, Dict2};
levenshtein_rec([FH], [SH], Dict) ->
  Dict2 = dict:store({FH, SH}, 1, Dict),
  {1, Dict2};
levenshtein_rec([], Second, Dict) ->
  Length = length(Second),
  Dict2 = dict:store({[], Second}, Length, Dict),
  {Length, Dict2};
levenshtein_rec(First, [], Dict) ->
  Length = length(First),
  Dict2 = dict:store({First, []}, Length, Dict),
  {Length, Dict2};
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
