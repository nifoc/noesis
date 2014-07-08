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