% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_proplists_test).

-include_lib("eunit/include/eunit.hrl").

get_keys_test() ->
  ?assertEqual([bar, foo, test], noesis_proplists:get_keys([{test, 1}, {foo, 2}, {bar, 3}])),
  ?assertEqual([bar, foo, test], noesis_proplists:get_keys([{test, 1}, {foo, 2}, {bar, 3}, {test, 4}])),
  ?assertEqual([<<"foo">>, <<"test">>], noesis_proplists:get_keys([{<<"test">>, 1}, {<<"foo">>, 2}])),
  ?assertEqual([], noesis_proplists:get_keys([])).

get_value_test() ->
  ?assertEqual(1, noesis_proplists:get_value(test, [{test, 1}, {foo, 2}, {bar, 3}])),
  ?assertEqual(3, noesis_proplists:get_value(<<"bar">>, [{<<"test">>, 1}, {<<"foo">>, 2}, {<<"bar">>, 3}])),
  ?assertEqual(undefined, noesis_proplists:get_value(test, [{foo, 2}, {bar, 3}])),
  ?assertEqual(null, noesis_proplists:get_value(test, [{foo, 2}, {bar, 3}], null)).

delete_keys_test() ->
  ?assertEqual([{foo, 2}, {bar, 3}], noesis_proplists:delete_keys([test], [{test, 1}, {foo, 2}, {bar, 3}])),
  ?assertEqual([{<<"test">>, 1}, {<<"bar">>, 3}], noesis_proplists:delete_keys([<<"foo">>], [{<<"test">>, 1}, {<<"foo">>, 2}, {<<"bar">>, 3}])),
  ?assertEqual([{<<"bar">>, 3}], noesis_proplists:delete_keys([<<"foo">>], [{<<"foo">>, 1}, {<<"foo">>, 2}, {<<"bar">>, 3}])),
  ?assertEqual([{test, 1}], noesis_proplists:delete_keys([], [{test, 1}])),
  ?assertEqual([], noesis_proplists:delete_keys([<<"foo">>], [])),
  ?assertEqual([], noesis_proplists:delete_keys([], [])).

keypos_test() ->
  ?assertEqual(1, noesis_proplists:keypos(test, [{test, "x"}, {foo, "y"}, {bar, "z"}])),
  ?assertEqual(3, noesis_proplists:keypos(<<"bar">>, [{<<"test">>, "x"}, {<<"foo">>, "y"}, {<<"bar">>, "z"}])),
  ?assertEqual(undefined, noesis_proplists:keypos(<<"test">>, [{<<"foo">>, "y"}, {<<"bar">>, "z"}])).

extract_test() ->
  ?assertEqual([{test, 1}], noesis_proplists:extract([test], [{test, 1}, {foo, 2}, {bar, 3}])),
  ?assertEqual([{bar, undefined}, {test, 1}], noesis_proplists:extract([test, bar], [{test, 1}, {foo, 2}])),
  ?assertEqual([{bar, null}, {test, 1}], noesis_proplists:extract([test, bar], [{test, 1}, {foo, 2}], null)),
  ?assertEqual([{bar, 99}, {test, 1}], noesis_proplists:extract([test, bar], [{test, 1}, {foo, 2}], null, [{bar, 99}])).

partial_extract_test() ->
  ?assertEqual([{test, 1}], noesis_proplists:partial_extract([test], [{test, 1}, {foo, 2}, {bar, 3}])),
  ?assertEqual([{test, 1}], noesis_proplists:partial_extract([test, bar], [{test, 1}, {foo, 2}])),
  ?assertEqual([{foo, 2}, {test, 1}], noesis_proplists:partial_extract([test, foo, bar], [{test, 1}, {foo, 2}, {baz, null}], null)).

merge_test() ->
  ?assertEqual([{a, 1}, {b, 2}], noesis_proplists:merge([{a, 1}], [{b, 2}])),
  ?assertEqual([{a, 1}, {b, 2}, {c, 3}], noesis_proplists:merge([{a, 1}, {c, 4}], [{b, 2}, {c, 3}])),
  ?assertEqual([{a, 1}, {b, 2}, {c, 4}], noesis_proplists:merge(fun(_K, V1, _V2) -> V1 end, [{a, 1}, {c, 4}], [{b, 2}, {c, 3}])),
  ?assertEqual([{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}], noesis_proplists:merge([{a, 1}, {c, 4}, {d, 4}], [{b, 2}, {c, 3}, {e, 5}])).
