% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_proplists_triq).

-import(triq, [numtests/2]).

-include_lib("triq/include/triq.hrl").

% Generators

proplist_key() -> binary().

proplist_value() -> int().

proplist() -> list({proplist_key(), proplist_value()}).

% Properties

prop_get_keys_1() ->
  ?FORALL(Proplist, proplist(),
    begin
      ListA = lists:usort(noesis_proplists:get_keys(Proplist)),
      ListB = lists:usort(proplists:get_keys(Proplist)),
      ListA =:= ListB
    end).

prop_get_value_1() ->
  numtests(200, ?FORALL({Key, Proplist}, {proplist_key(), proplist()},
    ?IMPLIES(proplists:is_defined(Key, Proplist),
      undefined =/= noesis_proplists:get_value(Key, Proplist)))).

prop_delete_keys_1() ->
  ?FORALL({Key, Proplist}, {proplist_key(), proplist()},
    not lists:member(Key, noesis_proplists:delete_keys([Key], Proplist))).

prop_keypos_1() ->
  numtests(200, ?FORALL({Key, Proplist}, {proplist_key(), proplist()},
    ?IMPLIES(proplists:is_defined(Key, Proplist),
      undefined =/= noesis_proplists:keypos(Key, Proplist)))).

prop_keypos_2() ->
  numtests(200, ?FORALL({Key, Proplist}, {proplist_key(), proplist()},
    ?IMPLIES(not proplists:is_defined(Key, Proplist),
      undefined =:= noesis_proplists:keypos(Key, Proplist)))).

prop_extract_1() ->
  ?FORALL({Keys, Proplist}, {list(proplist_key()), proplist()},
    length(Keys) >= length(noesis_proplists:extract(Keys, Proplist))).
