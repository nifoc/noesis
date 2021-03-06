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
% @doc The `noesis_xml' module provides functions for creating XML documents.

-module(noesis_xml).

% Types

-type key() :: string() | binary() | atom().
-type simple_value() :: string() | binary() | number().
-type value() :: simple_value() | elements().
-type attributes() :: noesis_proplists:proplist(key(), simple_value()).

-type option_cdata() :: {cdata, boolean()}.

-type option() :: option_cdata().
-type options() :: [option()].

-type element_simple() :: {key(), value()}.
-type element_attributes() :: {key(), attributes(), value()}.
-type element_attr_options() :: {key(), attributes(), value(), options()}.

-type element() :: element_simple() | element_attributes() | element_attr_options().

-type elements() :: [element()].

-export_type([
  key/0,
  value/0,
  attributes/0,
  options/0,
  element/0,
  elements/0
]).

% API
-export([
  from_list/1
]).

% API

% @doc Takes a list of {@link elements()} and returns a XML-binary.
-spec from_list(elements()) -> binary().
from_list(Elements) ->
  from_list_acc(Elements, []).

% Private

-spec from_list_acc(elements(), iolist()) -> binary().
from_list_acc([], Acc) ->
  iolist_to_binary(lists:reverse(Acc));
from_list_acc([Element|Rest], Acc) ->
  Acc2 = encode_element(Element, Acc),
  from_list_acc(Rest, Acc2).

-spec encode_element(element(), iolist()) -> iolist().
encode_element({Key, Value}, Acc) ->
  EncodedKey = encode_key(Key),
  EncodedValue = encode_value(Value),
  EncodedElement = [$<, EncodedKey, $>, EncodedValue, "</", EncodedKey, $>],
  [EncodedElement | Acc];
encode_element({Key, Attributes, Value}, Acc) ->
  EncodedKey = encode_key(Key),
  EncodedAttributes = encode_attributes(Attributes, []),
  EncodedValue = encode_value(Value),
  EncodedElement = [$<, EncodedKey, EncodedAttributes, $>, EncodedValue, "</", EncodedKey, $>],
  [EncodedElement | Acc];
encode_element({Key, Attributes, Value, Options}, Acc) ->
  EncodedKey = encode_key(Key),
  EncodedAttributes = encode_attributes(Attributes, []),
  EncodedValue = case noesis_proplists:get_value(cdata, Options, false) of
    true -> ["<![CDATA[", encode_value(Value), "]]>"];
    false -> encode_value(Value)
  end,
  EncodedElement = [$<, EncodedKey, EncodedAttributes, $>, EncodedValue, "</", EncodedKey, $>],
  [EncodedElement | Acc].

-spec encode_key(key()) -> iolist().
encode_key(Key) when is_atom(Key) ->
  atom_to_binary(Key, utf8);
encode_key(Key) ->
  Key.

-spec encode_attributes(attributes(), iolist()) -> iolist().
encode_attributes([], Acc) ->
  lists:reverse(Acc);
encode_attributes([{Key, Value}|Rest], Acc) ->
  EncodedKey = encode_key(Key), % Attr. keys are just like node keys
  EncodedValue = encode_value(Value), % Attr. value are simple node values
  EncodedAttribute = [" ", EncodedKey, $=, $", EncodedValue, $"],
  Acc2 = [EncodedAttribute | Acc],
  encode_attributes(Rest, Acc2).

-spec encode_value(value()) -> iolist().
encode_value(Value) when is_list(Value) andalso is_tuple(hd(Value)) ->
  from_list_acc(Value, []);
encode_value(Value) when is_integer(Value) ->
  integer_to_binary(Value);
encode_value(Value) when is_float(Value) ->
  float_to_binary(Value, [{decimals, 64}, compact]);
encode_value(Value) ->
  Value.

-ifdef(PERF).
horse_from_list() ->
  horse:repeat(100000,
    from_list([{foo, "bar"}, {xml, [{is, [{very, "enterprise"}], <<"!">>}]}, {<<"binary">>, "string"}])).
-endif.
