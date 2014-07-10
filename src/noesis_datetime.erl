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
% @doc The `noesis_datetime' module provides functions for working with dates and time.

-module(noesis_datetime).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Types

-type range_seconds() :: {non_neg_integer(), second} | {non_neg_integer(), seconds}.
-type range_minutes() :: {non_neg_integer(), minute} | {non_neg_integer(), minutes}.
-type range_hours() :: {non_neg_integer(), hour} | {non_neg_integer(), hours}.
-type range_days() :: {non_neg_integer(), day} | {non_neg_integer(), days}.
-type range_weeks() :: {non_neg_integer(), week} | {non_neg_integer(), weeks}.

-type timestamp() :: non_neg_integer().
-type comparison_op() :: lt | lte | eq | gt | gte.
-type range() :: range_seconds() | range_minutes() | range_hours() | range_days() | range_weeks().

-export_type([
  timestamp/0,
  comparison_op/0,
  range/0
]).

% API
-export([
  timestamp/0,
  local_timestamp/0,
  timestamp_distance/4,
  rfc1123/0,
  rfc1123/1
]).

% API

% @doc Returns the current Unix timestamp (UTC).
-spec timestamp() -> timestamp().
timestamp() ->
  {Mega, Secs, _} = os:timestamp(),
  Mega * 1000000 + Secs.

% @doc Returns the current Unix timestamp in the timezone of the system.
-spec local_timestamp() -> timestamp().
local_timestamp() ->
  Now = calendar:local_time(),
  UnixEpoch = 62167219200,
  LocalEpoch = calendar:datetime_to_gregorian_seconds(Now),
  LocalEpoch - UnixEpoch.

% @doc Takes two Unix timestamps and compares their distance to a given range.<br /><br />
%      <strong>Comparion operations</strong><br />
%      Less than: `lt'<br />
%      Less than or equal to: `lte'<br />
%      Equal: `eq'<br />
%      Greater than: `gt'<br />
%      Greater than or equal to: `gte'<br /><br />
%      <strong>Ranges</strong><br />
%      `{X, seconds}'<br />
%      `{X, minutes}'<br />
%      `{X, hours}'<br />
%      `{X, days}'<br />
%      `{X, weeks}'
-spec timestamp_distance(comparison_op(), timestamp(), timestamp(), range()) -> boolean().
timestamp_distance(Op, A, B, Range) when A > B ->
  timestamp_distance(Op, B, A, Range);
timestamp_distance(Op, A, B, Range) ->
  Seconds = range_to_seconds(Range),
  Diff = B - A,
  compare_timestamps(Op, Diff, Seconds).

% @doc Returns the current date and time (UTC) according to RFC 1123.
-spec rfc1123() -> binary().
rfc1123() ->
  Now = calendar:universal_time(),
  rfc1123(Now).

% @doc Formats any `calendar:datetime()' (UTC) according to RFC 1123.
-spec rfc1123(calendar:datetime()) -> binary().
rfc1123({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  Weekday = calendar:day_of_the_week({Year, Month, Day}),
  Formatted = io_lib:format("~s, ~2..0w ~s ~w ~2..0w:~2..0w:~2..0w GMT", [rfc1123_dayname(Weekday), Day, rfc1123_monthname(Month), Year, Hour, Minute, Second]),
  unicode:characters_to_binary(Formatted).

% Private

-spec range_to_seconds(range()) -> non_neg_integer().
range_to_seconds({X, R}) when R =:= seconds orelse R =:= second -> X;
range_to_seconds({X, R}) when R =:= minutes orelse R =:= minute -> 60 * X;
range_to_seconds({X, R}) when R =:= hours orelse R =:= hour -> 60 * 60 * X;
range_to_seconds({X, R}) when R =:= days orelse R =:= day -> 60 * 60 * 24 * X;
range_to_seconds({X, R}) when R =:= weeks orelse R =:= week -> 60 * 60 * 24 * 7 * X.

-spec compare_timestamps(comparison_op(), non_neg_integer(), non_neg_integer()) -> boolean().
compare_timestamps(lt, Diff, Seconds) -> Diff < Seconds;
compare_timestamps(lte, Diff, Seconds) -> Diff =< Seconds;
compare_timestamps(eq, Diff, Seconds) -> Diff =:= Seconds;
compare_timestamps(gt, Diff, Seconds) -> Diff > Seconds;
compare_timestamps(gte, Diff, Seconds) -> Diff >= Seconds.

-spec rfc1123_dayname(1..7) -> string().
rfc1123_dayname(1) -> "Mon";
rfc1123_dayname(2) -> "Tue";
rfc1123_dayname(3) -> "Wed";
rfc1123_dayname(4) -> "Thu";
rfc1123_dayname(5) -> "Fri";
rfc1123_dayname(6) -> "Sat";
rfc1123_dayname(7) -> "Sun".

-spec rfc1123_monthname(1..12) -> string().
rfc1123_monthname(1) -> "Jan";
rfc1123_monthname(2) -> "Feb";
rfc1123_monthname(3) -> "Mar";
rfc1123_monthname(4) -> "Apr";
rfc1123_monthname(5) -> "May";
rfc1123_monthname(6) -> "Jun";
rfc1123_monthname(7) -> "Jul";
rfc1123_monthname(8) -> "Aug";
rfc1123_monthname(9) -> "Sep";
rfc1123_monthname(10) -> "Oct";
rfc1123_monthname(11) -> "Nov";
rfc1123_monthname(12) -> "Dec".

-ifdef(TEST).
rfc1123_dayname_test() ->
  ?assertEqual(rfc1123_dayname(1), "Mon"),
  ?assertEqual(rfc1123_dayname(2), "Tue"),
  ?assertEqual(rfc1123_dayname(3), "Wed"),
  ?assertEqual(rfc1123_dayname(4), "Thu"),
  ?assertEqual(rfc1123_dayname(5), "Fri"),
  ?assertEqual(rfc1123_dayname(6), "Sat"),
  ?assertEqual(rfc1123_dayname(7), "Sun").

rfc1123_monthname_test() ->
  ?assertEqual(rfc1123_monthname(1), "Jan"),
  ?assertEqual(rfc1123_monthname(2), "Feb"),
  ?assertEqual(rfc1123_monthname(3), "Mar"),
  ?assertEqual(rfc1123_monthname(4), "Apr"),
  ?assertEqual(rfc1123_monthname(5), "May"),
  ?assertEqual(rfc1123_monthname(6), "Jun"),
  ?assertEqual(rfc1123_monthname(7), "Jul"),
  ?assertEqual(rfc1123_monthname(8), "Aug"),
  ?assertEqual(rfc1123_monthname(9), "Sep"),
  ?assertEqual(rfc1123_monthname(10), "Oct"),
  ?assertEqual(rfc1123_monthname(11), "Nov"),
  ?assertEqual(rfc1123_monthname(12), "Dec").
-endif.
