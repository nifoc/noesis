% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_datetime_test).

-include_lib("eunit/include/eunit.hrl").

timestamp_test() ->
  ?assert(noesis_datetime:timestamp() > 0),
  TimeA = noesis_datetime:timestamp(),
  ok = timer:sleep(1100),
  TimeB = noesis_datetime:timestamp(),
  ?assert(TimeA < TimeB).

local_timestamp_test() ->
  ?assert(noesis_datetime:local_timestamp() > 0),
  TimeA = noesis_datetime:local_timestamp(),
  ok = timer:sleep(1100),
  TimeB = noesis_datetime:local_timestamp(),
  ?assert(TimeA < TimeB).

timestamp_to_datetime_test() ->
  ?assertEqual({{2014, 7, 12},{22, 7, 28}}, noesis_datetime:timestamp_to_datetime(1405202848)).

timestamp_distance_test() ->
  ?assert(noesis_datetime:timestamp_distance(gte, 1357084799, 1357171199, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(gte, 1357171199, 1357084799, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(lte, 1357084799, 1357171199, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(lte, 1357171199, 1357084799, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(eq, 1357084799, 1357171199, {1, day})),
  ?assertNot(noesis_datetime:timestamp_distance(eq, 1357084799, 1357171198, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(lt, 1357084799, 1357171198, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(gt, 1357084799, 1357171200, {1, day})),
  ?assert(noesis_datetime:timestamp_distance(eq, 1357084798, 1357084799, {1, second})),
  ?assert(noesis_datetime:timestamp_distance(eq, 1357084798, 1357084858, {1, minute})),
  ?assert(noesis_datetime:timestamp_distance(eq, 1357084798, 1357091998, {2, hours})),
  ?assert(noesis_datetime:timestamp_distance(eq, 1357084798, 1359503998, {4, weeks})),
  ?assert(noesis_datetime:timestamp_distance(gt, 1357084798, 1359503999, {4, weeks})).

rfc1123_test() ->
  TimeA = noesis_datetime:rfc1123(),
  ok = timer:sleep(1100),
  TimeB = noesis_datetime:rfc1123(),
  ?assert(TimeA =/= TimeB).

rfc1123_to_datetime_test() ->
  Time = calendar:universal_time(),
  Rfc1123 = noesis_datetime:rfc1123(Time),
  ?assertEqual(Time, noesis_datetime:rfc1123_to_datetime(Rfc1123)).

iso8601_to_datetime_test() ->
  TimeA = <<"2014-07-12T04:24:26Z">>,
  TimeB = <<"2014-07-12 04:24:26">>,
  ?assertEqual({{2014, 7, 12}, {4, 24, 26}}, noesis_datetime:iso8601_to_datetime(TimeA)),
  ?assertEqual({{2014, 7, 12}, {4, 24, 26}}, noesis_datetime:iso8601_to_datetime(TimeB)),
  ?assertEqual(noesis_datetime:iso8601_to_datetime(TimeA), noesis_datetime:iso8601_to_datetime(TimeB)).
