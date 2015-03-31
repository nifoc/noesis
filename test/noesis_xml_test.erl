% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_xml_test).

-include_lib("eunit/include/eunit.hrl").

from_list_test() ->
  ?assertEqual(<<>>, noesis_xml:from_list([])),
  ?assertEqual(<<"<binary>test</binary>">>, noesis_xml:from_list([{binary, <<"test">>}])),
  ?assertEqual(<<"<string>test</string>">>, noesis_xml:from_list([{"string", "test"}])),
  ?assertEqual(<<"<int>1</int>">>, noesis_xml:from_list([{"int", 1}])),
  ?assertEqual(<<"<float>1.5</float>">>, noesis_xml:from_list([{"float", 1.5}])),
  ?assertEqual(<<"<attr nice=\"meme\">value</attr>">>, noesis_xml:from_list([{"attr", [{nice, <<"meme">>}], "value"}])),
  ?assertEqual(
    <<"<foo>bar</foo><xml><is very=\"enterprise\">!</is></xml><binary>string</binary>">>,
    noesis_xml:from_list([{foo, "bar"}, {xml, [{is, [{very, "enterprise"}], <<"!">>}]}, {<<"binary">>, "string"}])),
  ?assertEqual(<<"<cdata><![CDATA[<x>]]></cdata>">>, noesis_xml:from_list([{"cdata", [], "<x>", [{cdata, true}]}])),
  ?assertEqual(<<"<cdata>x</cdata>">>, noesis_xml:from_list([{"cdata", [], "x", [{cdata, false}]}])).
