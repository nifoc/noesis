% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_xml_triq).

-include_lib("triq/include/triq.hrl").

-import(triq, [numtests/2]).

% Generators

string() -> [char()].

float() -> ?LET({A, B}, {int(), pos_integer()}, A / B * 100).

non_empty_binary() -> ?SUCHTHAT(B, binary(), byte_size(B) > 0).

non_empty_atom() -> ?SUCHTHAT(A, atom(), byte_size(atom_to_binary(A, utf8)) > 0).

non_empty_string() -> ?SUCHTHAT(S, string(), length(S) > 0).

list_key() -> oneof([non_empty_binary(), non_empty_atom(), non_empty_string()]).

list_value_simple() -> oneof([binary(), string(), int(), float()]).

list_value() -> oneof([list_value_simple(), [{list_key(), list_value_simple()}]]).

proplist_simple() -> list({list_key(), list_value()}).

proplist_simple_string() -> list({non_empty_string(), non_empty_string()}).

proplist_attributes() -> list({list_key(), [{list_key(), list_value_simple()}], list_value()}).

proplist_attr_options() -> list({list_key(), [{list_key(), list_value_simple()}], list_value(), []}).

proplist() -> oneof([proplist_simple(), proplist_attributes(), proplist_attr_options()]).

% Properties

prop_from_list_1() ->
  numtests(200, ?FORALL(List, proplist(),
    is_binary(noesis_xml:from_list(List)))).

prop_from_list_2() ->
  ?FORALL(List, proplist_simple_string(),
    ?IMPLIES(length(List) > 0,
      begin
        Xml = unicode:characters_to_list(noesis_xml:from_list(List)),
        {Doc, _Rest} = xmerl_scan:string(Xml),
        is_tuple(Doc)
      end)).
