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
  cos/1,
  pow/2,
  sin/1,
  sqrt/1
]).

-define(PI, math:pi()).
-define(R, 6372.8). % Earth radius

% Types

-type coordinates() :: {Longitude :: number(), Latitude :: number()}.

-export_type([
  coordinates/0
]).

% API
-export([
  lat/1,
  lng/1,
  haversine/2,
  deg2rad/1,
  rad2deg/1
]).

% API

% @doc Returns the latitude value of a `coordinates()' tuple.
-spec lat(coordinates()) -> number().
lat({_Lng, Lat}) -> Lat.

% @doc Returns the longitude value of a `coordinates()' tuple.
-spec lng(coordinates()) -> number().
lng({Lng, _Lat}) -> Lng.

% @doc Calculates the great-circle distance between two coordinates, that is the shortest distance between
%      two points on the surface of a sphere.
-spec haversine(coordinates(), coordinates()) -> number().
haversine({StartLng, StartLat}, {EndLng, EndLat}) ->
  DLng = deg2rad(EndLng - StartLng),
  DLat = deg2rad(EndLat - StartLat),
  RadStartLat = deg2rad(StartLat),
  RadEndLat = deg2rad(EndLat),
  A = pow(sin(DLat / 2), 2) + cos(RadStartLat) * cos(RadEndLat) * pow(sin(DLng / 2), 2),
  C = 2 * asin(sqrt(A)),
  ?R * C.

% @doc Converts degrees to radians.
-spec deg2rad(number() | coordinates()) -> number() | coordinates().
deg2rad({Lng, Lat}) -> {deg2rad(Lng), deg2rad(Lat)};
deg2rad(Deg) -> ?PI * Deg / 180.

% @doc Converts radians to degrees.
-spec rad2deg(number() | coordinates()) -> number() | coordinates().
rad2deg({Lng, Lat}) -> {rad2deg(Lng), rad2deg(Lat)};
rad2deg(Rad) -> 180 * Rad / ?PI.
