% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_polyline_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
  ?assertEqual(<<"_p~iF~ps|U_ulLnnqC_mqNvxq`@">>, noesis_polyline:encode([{-120.2, 38.5}, {-120.95, 40.7}, {-126.453, 43.252}])),
  ?assertEqual(<<"}wjiGtdpcNrAlBJZ">>, noesis_polyline:encode([{-79.38651, 43.64175}, {-79.38706, 43.64133}, {-79.3872, 43.64127}])),
  ?assertEqual(<<"_ojiHa`tLh{IdCw{Gwc_@">>, noesis_polyline:encode([{2.23761, 48.8832}, {2.23694, 48.82747}, {2.40154, 48.87303}])).

decode_test() ->
  ?assertEqual([{-120.2, 38.5}, {-120.95, 40.7}, {-126.453, 43.252}], noesis_polyline:decode(<<"_p~iF~ps|U_ulLnnqC_mqNvxq`@">>)),
  ?assertEqual([{-79.38651, 43.64175}, {-79.38706, 43.64133}, {-79.3872, 43.64127}], noesis_polyline:decode(<<"}wjiGtdpcNrAlBJZ">>)),
  ?assertEqual([{2.23761, 48.8832}, {2.23694, 48.82747}, {2.40154, 48.87303}], noesis_polyline:decode(<<"_ojiHa`tLh{IdCw{Gwc_@">>)).
