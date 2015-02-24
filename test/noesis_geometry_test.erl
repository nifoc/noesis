% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_geometry_test).

-include_lib("eunit/include/eunit.hrl").

lat_test() ->
  ?assertEqual(1, noesis_geometry:lat({9001, 1})).

lng_test() ->
  ?assertEqual(1, noesis_geometry:lng({1, 9001})).

haversine_test() ->
  ?assertEqual(2887.2599506071106, noesis_geometry:haversine({-86.67, 36.12}, {-118.40, 33.94})),
  ?assertEqual(170.3043950254226, noesis_geometry:haversine({-1.8494, 53.1472}, {0.1406, 52.2044})).

deg2rad_test() ->
  ?assertEqual(0.017453292519943295, noesis_geometry:deg2rad(1)),
  ?assertEqual(1.0, noesis_geometry:deg2rad(57.29577951308232)),
  ?assertEqual({0.017453292519943295, 1.0}, noesis_geometry:deg2rad({1, 57.29577951308232})).

rad2deg_test() ->
  ?assertEqual(57.29577951308232, noesis_geometry:rad2deg(1)),
  ?assertEqual(1.0, noesis_geometry:rad2deg(0.017453292519943295)),
  ?assertEqual({1.0, 57.29577951308232}, noesis_geometry:rad2deg({0.017453292519943295, 1})).
