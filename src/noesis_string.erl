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
  Cache = dict:new(),
  {Distance, _Cache2} = levenshtein_rec(First, Second, Cache),
  Distance.

% Private

-spec levenshtein_rec(string(), string(), noesis_dict()) -> {non_neg_integer(), noesis_dict()}.
levenshtein_rec(Str, Str, Cache) ->
  Cache2 = dict:store({Str, Str}, 0, Cache),
  {0, Cache2};
levenshtein_rec([FH], [SH], Cache) ->
  Cache2 = dict:store({FH, SH}, 1, Cache),
  {1, Cache2};
levenshtein_rec([], Second, Cache) ->
  Length = length(Second),
  Cache2 = dict:store({[], Second}, Length, Cache),
  {Length, Cache2};
levenshtein_rec(First, [], Cache) ->
  Length = length(First),
  Cache2 = dict:store({First, []}, Length, Cache),
  {Length, Cache2};
levenshtein_rec([FH|FT]=First, [SH|ST]=Second, Cache) ->
  case dict:is_key({First, Second}, Cache) of
    true ->
      Length = dict:fetch({First, Second}, Cache),
      {Length, Cache};
    false ->
      {Distance1, Cache2} = levenshtein_rec(First, ST, Cache),
      {Distance2, Cache3} = levenshtein_rec(FT, Second, Cache2),
      {Distance3A, Cache4} = levenshtein_rec(FT, ST, Cache3),
      {Distance3B, Cache5} = levenshtein_rec([FH], [SH], Cache4),
      Length = lists:min([Distance1 + 1, Distance2 + 1, Distance3A + Distance3B]),
      Cache6 = dict:store({First, Second}, Length, Cache5),
      {Length, Cache6}
  end.
