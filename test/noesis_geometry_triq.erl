% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_geometry_triq).

-include_lib("triq/include/triq.hrl").

-import(triq, [numtests/2]).

% Generators

latitude_float() -> ?LET({A, B}, {int(), pos_integer()}, noesis_geometry:normalize_lat(A / B * 100)).

longitude_float() -> ?LET({A, B}, {int(), pos_integer()}, noesis_geometry:normalize_lng(A / B * 100)).

point() -> {longitude_float(), latitude_float()}.

bounds() -> {point(), point()}.

bearing() -> ?LET(B, int(), noesis_geometry:normalize_bearing(B)).

% Properties

prop_contains_point_1() ->
  ?FORALL({Bounds, Point}, {bounds(), point()},
    is_boolean(noesis_geometry:contains_point(Bounds, Point))).

prop_extend_1() ->
  ?FORALL({Bounds, Point}, {bounds(), point()},
    is_tuple(noesis_geometry:extend(Bounds, Point))).

prop_extend_2() ->
  ?FORALL({Bounds, Point}, {bounds(), point()},
    begin
      ExtendedBounds = noesis_geometry:extend(Bounds, Point),
      noesis_geometry:contains_point(ExtendedBounds, Point) =:= true
    end).

prop_distance_1() ->
  ?FORALL({Start, Dest}, {point(), point()},
    is_number(noesis_geometry:distance(Start, Dest))).

prop_rhumb_distance_1() ->
  numtests(200, ?FORALL({Start, Dest}, {point(), point()},
    is_number(noesis_geometry:rhumb_distance(Start, Dest)))).

prop_rhumb_destination_point_1() ->
  numtests(200, ?FORALL({Point, Bearing, Distance}, {point(), bearing(), pos_integer()},
    is_tuple(noesis_geometry:rhumb_destination_point(Point, Bearing, Distance)))).

prop_rhumb_bearing_to_1() ->
  numtests(200, ?FORALL({StartPoint, DestPoint}, {point(), point()},
    is_float(noesis_geometry:rhumb_bearing_to(StartPoint, DestPoint)))).
