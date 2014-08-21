% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_datetime_triq).

-include_lib("triq/include/triq.hrl").

% Generators

year_int() -> ?SUCHTHAT(I, int(), (I >= 0) and (I =< 9999)).

month_int() -> ?SUCHTHAT(I, int(), (I >= 1) and (I =< 12)).

day_int() -> ?SUCHTHAT(I, int(), (I >= 1) and (I =< 28)).

hour_int() -> ?SUCHTHAT(I, int(), (I >= 0) and (I =< 23)).

minute_int() -> ?SUCHTHAT(I, int(), (I >= 0) and (I =< 59)).

second_int() -> ?SUCHTHAT(I, int(), (I >= 0) and (I =< 59)).

year_binary_digit() -> ?LET(I, year_int(), iolist_to_binary(io_lib:format("~4..0w", [I]))).

month_binary_digit() -> ?LET(I, month_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

day_binary_digit() -> ?LET(I, day_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

hour_binary_digit() -> ?LET(I, hour_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

minute_binary_digit() -> ?LET(I, minute_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

second_binary_digit() -> ?LET(I, second_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

iso8601_tuple() -> {{year_int(), month_int(), day_int()}, {hour_int(), minute_int(), second_int()}}.

iso8601_binary() -> ?LET({Y, Mo, D, H, Mi, S},
                         {year_binary_digit(), month_binary_digit(), day_binary_digit(), hour_binary_digit(), minute_binary_digit(), second_binary_digit()},
                         <<Y/binary, "-", Mo/binary, "-", D/binary, " ", H/binary, ":", Mi/binary, ":", S/binary>>
                        ).

% Properties

prop_iso8601_1() ->
  ?FORALL(Tuple, iso8601_tuple(),
    is_binary(noesis_datetime:iso8601(Tuple))).

prop_iso8601_2() ->
  ?FORALL(Tuple, iso8601_tuple(),
    begin
      Bin = noesis_datetime:iso8601(Tuple),
      Tuple =:= noesis_datetime:iso8601_to_datetime(Bin)
    end).

prop_iso8601_to_datetime_1() ->
  ?FORALL(Bin, iso8601_binary(),
    is_tuple(noesis_datetime:iso8601_to_datetime(Bin))).

prop_iso8601_to_datetime_2() ->
  ?FORALL(Bin, iso8601_binary(),
    calendar:valid_date(element(1, noesis_datetime:iso8601_to_datetime(Bin)))).
