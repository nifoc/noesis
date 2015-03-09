% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
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
% @doc The `noesis_geometry' module provides functions (more or less) related to geometry.

-module(noesis_geometry).

-import(math, [
  asin/1,
  atan2/2,
  cos/1,
  log/1,
  pow/2,
  sin/1,
  sqrt/1,
  tan/1
]).

-import(noesis_math, [
  fmod/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(PI, 3.141592653589793). % math:pi()
-define(PI_HALF, 1.5707963267948966). % math:pi() / 2
-define(PI_FOURTH, 0.7853981633974483). % math:pi() / 4
-define(R, 6372.8). % Earth radius

% Types

-type coordinates() :: {Longitude :: number(), Latitude :: number()}.
-type path() :: [coordinates()].
-type bounds() :: {NorthEast :: coordinates(), SouthWest :: coordinates()}.

-export_type([
  coordinates/0,
  path/0,
  bounds/0
]).

% API
-export([
  lat/1,
  lng/1,
  north_east/1,
  south_west/1,
  center/1,
  crosses_antimeridian/1,
  distance/2,
  rhumb_distance/2,
  rhumb_destination_point/3,
  rhumb_bearing_to/2,
  deg2rad/1,
  rad2deg/1,
  normalize_lat/1,
  normalize_lng/1,
  normalize_bearing/1
]).

% API

% @doc Returns the latitude value of a `coordinates()' tuple.
-spec lat(coordinates()) -> number().
lat({_Lng, Lat}) -> Lat.

% @doc Returns the longitude value of a `coordinates()' tuple.
-spec lng(coordinates()) -> number().
lng({Lng, _Lat}) -> Lng.

% @doc Returns the NE value of a `bounds()' tuple.
-spec north_east(bounds()) -> coordinates().
north_east({NE, _SW}) -> NE.

% @doc Returns the SW value of a `bounds()' tuple.
-spec south_west(bounds()) -> coordinates().
south_west({_NE, SW}) -> SW.

% @doc Calculates the center point of a `bounds()' tuple.
-spec center(bounds()) -> coordinates().
center({{NELng, NELat}, {SWLng, SWLat}}=Bounds) ->
  Lng = case crosses_antimeridian(Bounds) of
    true ->
      Span = lng_span(SWLng, NELng),
      normalize_lng(SWLng + Span / 2);
    false -> (SWLng + NELng) / 2
  end,
  Lat = (SWLat + NELat) / 2,
  {Lng, Lat}.

% @doc Returns whether or not the bounds intersect the antimeridian.
-spec crosses_antimeridian(bounds()) -> boolean().
crosses_antimeridian({{NELng, _NELat}, {SWLng, _SWLat}}) -> SWLng > NELng.

% @doc Calculates the great-circle distance between two coordinates, that is the shortest distance between
%      two points on the surface of a sphere.<br />
%      `StartLng', `StartLat', `EndLng' and `EndLat' are all expected to be in degrees.
-spec distance(coordinates(), coordinates()) -> number().
distance({StartLng, StartLat}, {DestLng, DestLat}) ->
  DLng = deg2rad(DestLng - StartLng),
  DLat = deg2rad(DestLat - StartLat),
  RadStartLat = deg2rad(StartLat),
  RadDestLat = deg2rad(DestLat),
  A = pow(sin(DLat / 2), 2) + cos(RadStartLat) * cos(RadDestLat) * pow(sin(DLng / 2), 2),
  C = 2 * asin(sqrt(A)),
  ?R * C.

% @doc Given a starting point and a destination point, this will calculate the distance between the two.<br />
%      `StartPoint' and `DestPoint' are both expected to be in degrees.<br /><br />
%      Partially based on <a href="http://www.movable-type.co.uk/scripts/latlong.html">Movable Type Scripts</a> by Chris Veness.
-spec rhumb_distance(coordinates(), coordinates()) -> number().
rhumb_distance({StartLng, StartLat}, {DestLng, DestLat}) ->
  DLng = deg2rad(abs(DestLng - StartLng)),
  DLat = deg2rad(DestLat - StartLat),
  RadStartLat = deg2rad(StartLat),
  RadDestLat = deg2rad(DestLat),
  DPsi = log(tan(RadDestLat / 2 + ?PI_FOURTH) / tan(RadStartLat / 2 + ?PI_FOURTH)),
  Q = rhumb_calculate_q(DLat, DPsi, RadStartLat),
  DLng2 = rhumb_bounds_check((abs(DLng) > ?PI), -(2 * ?PI - DLng), (2 * ?PI + DLng), DLng),
  Delta = sqrt(DLat * DLat + Q * Q * DLng2 * DLng2),
  ?R * Delta.

% @doc Given a starting point, a bearing and a distance, this will calculate the destination point.
%      If you maintain a constant bearing along a rhumb line, you will gradually spiral in towards one of the poles.<br />
%      `Point' and `Bearing' are both expected to be in degrees. `Distance' is expected to be in kilometers.<br /><br />
%      Based on <a href="http://www.movable-type.co.uk/scripts/latlong.html">Movable Type Scripts</a> by Chris Veness.
-spec rhumb_destination_point(coordinates(), number(), number()) -> coordinates().
rhumb_destination_point(Point, Bearing, Distance) ->
  D = Distance / ?R,
  {RadLng, RadLat} = deg2rad(Point),
  RadBrearing = deg2rad(Bearing),
  DestLat = RadLat + D * cos(RadBrearing),
  DLat = DestLat - RadLat,
  DPsi = log(tan(DestLat / 2 + ?PI_FOURTH) / tan(RadLat / 2 + ?PI_FOURTH)),
  Q = rhumb_calculate_q(DLat, DPsi, RadLat),
  DLng = D * sin(RadBrearing) / Q,
  DestLat2 = rhumb_bounds_check((abs(DestLat) > ?PI_HALF), (?PI - DestLat), -(?PI - DestLat), DestLat),
  DestLng = fmod((RadLng + DLng + ?PI), (2 * ?PI)) - ?PI,
  {rad2deg(DestLng), rad2deg(DestLat2)}.

% @doc Given a starting point and a destination point, this will calculate the bearing between the two.<br />
%      `StartPoint' and `DestPoint' are both expected to be in degrees.<br /><br />
%      Partially based on <a href="http://www.movable-type.co.uk/scripts/latlong.html">Movable Type Scripts</a> by Chris Veness.
-spec rhumb_bearing_to(coordinates(), coordinates()) -> float().
rhumb_bearing_to(StartPoint, DestPoint) ->
  {RadStartLng, RadStartLat} = deg2rad(StartPoint),
  {RadDestLng, RadDestLat} = deg2rad(DestPoint),
  DLng = RadDestLng - RadStartLng,
  DPsi = log(tan(RadDestLat / 2 + ?PI_FOURTH) / tan(RadStartLat / 2 + ?PI_FOURTH)),
  DLng2 = rhumb_bounds_check((abs(DLng) > ?PI), -(2 * ?PI - DLng), (2 * ?PI + DLng), DLng),
  Bearing = rad2deg(atan2(DLng2, DPsi)),
  normalize_bearing(Bearing).

% @doc Converts degrees to radians.
-spec deg2rad(number() | coordinates()) -> number() | coordinates().
deg2rad({Lng, Lat}) -> {deg2rad(Lng), deg2rad(Lat)};
deg2rad(Deg) -> ?PI * Deg / 180.

% @doc Converts radians to degrees.
-spec rad2deg(number() | coordinates()) -> number() | coordinates().
rad2deg({Lng, Lat}) -> {rad2deg(Lng), rad2deg(Lat)};
rad2deg(Rad) -> 180 * Rad / ?PI.

% @doc Normalizes a latitude to the `[-90, 90]' range. Latitudes above 90 or below -90 are capped, not wrapped.
-spec normalize_lat(number()) -> float().
normalize_lat(Lat) -> float(max(-90, min(90, Lat))).

% @doc Normalizes a longitude to the `[-180, 180]' range. Longitudes above 180 or below -180 are wrapped.
-spec normalize_lng(number()) -> float().
normalize_lng(Lng) ->
  Lng2 = fmod(Lng, 360),
  normalize_lng2(Lng2).

% @doc Normalizes a bearing to the `[0, 360]' range. Bearings above 360 or below 0 are wrapped.
-spec normalize_bearing(number()) -> float().
normalize_bearing(Bearing) -> fmod((fmod(Bearing, 360) + 360), 360).

% Private

-spec lng_span(number(), number()) -> number().
lng_span(West, East) when West > East -> East + 360 - West;
lng_span(West, East) -> East - West.

-spec normalize_lng2(number()) -> float().
normalize_lng2(Lng) when Lng == 180 -> 180.0;
normalize_lng2(Lng) when Lng < -180 -> Lng + 360.0;
normalize_lng2(Lng) when Lng > 180 -> Lng - 360.0;
normalize_lng2(Lng) -> float(Lng).

-spec rhumb_bounds_check(boolean(), number(), number(), number()) -> number().
rhumb_bounds_check(true, GtZero, _LteZero, Default) when Default > 0 -> GtZero;
rhumb_bounds_check(true, _GtZero, LteZero, _Default) -> LteZero;
rhumb_bounds_check(false, _GtZero, _LteZero, Default) -> Default.

-spec rhumb_calculate_q(number(), number(), number()) -> number().
rhumb_calculate_q(DLat, DPsi, RadLat) ->
  try (DLat / DPsi)
  catch
    error:_ -> cos(RadLat)
  end.

% Tests (private functions)

-ifdef(TEST).
rhumb_bounds_check_test() ->
  ?assertEqual(9001, rhumb_bounds_check(true, 9001, 0, 1)),
  ?assertEqual(9001, rhumb_bounds_check(true, 0, 9001, 0)),
  ?assertEqual(9001, rhumb_bounds_check(false, 0, 0, 9001)).

rhumb_calculate_q_test() ->
  ?assertEqual(2.0, rhumb_calculate_q(10, 5, 9001)),
  ?assertEqual(1.0, rhumb_calculate_q(9001, 0, 0)).
-endif.
