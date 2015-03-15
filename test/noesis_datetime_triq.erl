% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
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

month_int() -> oneof(lists:seq(1, 12)).

day_int() -> oneof(lists:seq(1, 28)).

hour_int() -> oneof(lists:seq(0, 23)).

minute_int() -> oneof(lists:seq(0, 59)).

second_int() -> oneof(lists:seq(0, 59)).

year_binary_digit() -> ?LET(I, year_int(), iolist_to_binary(io_lib:format("~4..0w", [I]))).

month_binary_digit() -> ?LET(I, month_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

day_binary_digit() -> ?LET(I, day_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

hour_binary_digit() -> ?LET(I, hour_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

minute_binary_digit() -> ?LET(I, minute_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

second_binary_digit() -> ?LET(I, second_int(), iolist_to_binary(io_lib:format("~2..0w", [I]))).

month_binary_word() -> oneof([<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                              <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).

day_binary_word() -> oneof([<<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>, <<"Fri">>, <<"Sat">>, <<"Sun">>]).

datetime() -> {{year_int(), month_int(), day_int()}, {hour_int(), minute_int(), second_int()}}.

rfc1123() -> ?LET({WD, D, Mo, Y, H, Mi, S},
                         {day_binary_word(), day_binary_digit(), month_binary_word(), year_binary_digit(),
                          hour_binary_digit(), minute_binary_digit(), second_binary_digit()},
                         <<WD/binary, ", ", D/binary, " ", Mo/binary, " ", Y/binary, " ", H/binary, ":", Mi/binary, ":", S/binary, " GMT">>).

iso8601() -> ?LET({Y, Mo, D, H, Mi, S},
                         {year_binary_digit(), month_binary_digit(), day_binary_digit(), hour_binary_digit(), minute_binary_digit(), second_binary_digit()},
                         <<Y/binary, "-", Mo/binary, "-", D/binary, "T", H/binary, ":", Mi/binary, ":", S/binary, "Z">>).

% Properties

prop_rfc1123_1() ->
  ?FORALL(Tuple, datetime(),
    is_binary(noesis_datetime:rfc1123(Tuple))).

prop_rfc1123_2() ->
  ?FORALL(Tuple, datetime(),
    begin
      Bin = noesis_datetime:rfc1123(Tuple),
      Tuple =:= noesis_datetime:rfc1123_to_datetime(Bin)
    end).

prop_rfc1123_to_datetime_1() ->
  ?FORALL(Bin, rfc1123(),
    is_tuple(noesis_datetime:rfc1123_to_datetime(Bin))).

prop_rfc1123_to_datetime_2() ->
  ?FORALL(Bin, rfc1123(),
    calendar:valid_date(element(1, noesis_datetime:rfc1123_to_datetime(Bin)))).

prop_iso8601_1() ->
  ?FORALL(Tuple, datetime(),
    is_binary(noesis_datetime:iso8601(Tuple))).

prop_iso8601_2() ->
  ?FORALL(Tuple, datetime(),
    begin
      Bin = noesis_datetime:iso8601(Tuple),
      Tuple =:= noesis_datetime:iso8601_to_datetime(Bin)
    end).

prop_iso8601_to_datetime_1() ->
  ?FORALL(Bin, iso8601(),
    is_tuple(noesis_datetime:iso8601_to_datetime(Bin))).

prop_iso8601_to_datetime_2() ->
  ?FORALL(Bin, iso8601(),
    calendar:valid_date(element(1, noesis_datetime:iso8601_to_datetime(Bin)))).
