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

-type timestamp() :: non_neg_integer().

-export_type([
  timestamp/0
]).

% API
-export([
  timestamp/0,
  local_timestamp/0
]).

% API

% @doc Returns the current UTC Unix timestamp.<br />
-spec timestamp() -> timestamp().
timestamp() ->
  {Mega, Secs, _} = os:timestamp(),
  Mega * 1000000 + Secs.

% doc Returns the current Unix timestamp in the timezone of the system.
-spec local_timestamp() -> timestamp().
local_timestamp() ->
  Now = calendar:local_time(),
  UnixEpoch = 62167219200,
  LocalEpoch = calendar:datetime_to_gregorian_seconds(Now),
  LocalEpoch - UnixEpoch.
