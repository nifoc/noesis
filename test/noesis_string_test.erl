% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_string_test).

-include_lib("eunit/include/eunit.hrl").

levenshtein_test_() ->
  {timeout, 60, fun() ->
    ?assertEqual(3, noesis_string:levenshtein("kitten", "sitting")),
    ?assertEqual(2, noesis_string:levenshtein("Tor", "Tier")),
    ?assertEqual(1, noesis_string:levenshtein("carrrot", "carrot")),
    ?assertEqual(9, noesis_string:levenshtein("hallo", "informatik")),
    ?assertEqual(7, noesis_string:levenshtein("Germany", "Argentina")),
    ?assertEqual(6, noesis_string:levenshtein("Cobol", "Erlang")),
    ?assertEqual(19, noesis_string:levenshtein("Ein kleiner Satz", "Dieser Satz ist laenger"))
  end}.

hamming_test() ->
  ?assertEqual(3, noesis_string:hamming("karolin", "kathrin")),
  ?assertEqual(3, noesis_string:hamming("karolin", "kerstin")),
  ?assertEqual(2, noesis_string:hamming("1011101", "1001001")),
  ?assertEqual(3, noesis_string:hamming("2173896", "2233796")).
