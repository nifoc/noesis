% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_math_test).

-include_lib("eunit/include/eunit.hrl").

fmod_test() ->
  ?assertEqual(2.0, noesis_math:fmod(7, 5)),
  ?assertEqual(0.09999999999999998, noesis_math:fmod(0.9, 0.2)).

floor_test() ->
  ?assertEqual(4, noesis_math:floor(4.2)),
  ?assertEqual(4, noesis_math:floor(4.6)),
  ?assertEqual(-5, noesis_math:floor(-4.2)),
  ?assertEqual(-5, noesis_math:floor(-4.6)),
  ?assertEqual(100, noesis_math:floor(100.499999)).

ceiling_test() ->
  ?assertEqual(5, noesis_math:ceiling(4.2)),
  ?assertEqual(5, noesis_math:ceiling(4.6)),
  ?assertEqual(-4, noesis_math:ceiling(-4.2)),
  ?assertEqual(-4, noesis_math:ceiling(-4.6)),
  ?assertEqual(101, noesis_math:ceiling(100.499999)).
