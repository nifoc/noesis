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

% Types

-type range_days() :: {non_neg_integer(), day} | {non_neg_integer(), days}.

-type timestamp() :: non_neg_integer().
-type comparison_op() :: lt | lte | eq | gt | gte.
-type range() :: range_days().

-export_type([
  timestamp/0,
  comparison_op/0,
  range/0
]).

% API
-export([
  timestamp/0,
  local_timestamp/0,
  timestamp_distance/4
]).

% API

% @doc Returns the current UTC Unix timestamp.
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
%      `{X, days}'
-spec timestamp_distance(comparison_op(), timestamp(), timestamp(), range()) -> boolean().
timestamp_distance(Op, A, B, Range) when A > B ->
  timestamp_distance(Op, B, A, Range);
timestamp_distance(Op, A, B, Range) ->
  Seconds = range_to_seconds(Range),
  Diff = B - A,
  compare_timestamps(Op, Diff, Seconds).

% Private

-spec range_to_seconds(range()) -> non_neg_integer().
range_to_seconds({X, R}) when R =:= days orelse R =:= day -> 60 * 60 * 24 * X.

-spec compare_timestamps(comparison_op(), non_neg_integer(), non_neg_integer()) -> boolean().
compare_timestamps(lt, Diff, Seconds) -> Diff < Seconds;
compare_timestamps(lte, Diff, Seconds) -> Diff =< Seconds;
compare_timestamps(eq, Diff, Seconds) -> Diff =:= Seconds;
compare_timestamps(gt, Diff, Seconds) -> Diff > Seconds;
compare_timestamps(gte, Diff, Seconds) -> Diff >= Seconds.
