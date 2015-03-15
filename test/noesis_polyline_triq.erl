% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_polyline_triq).

-include_lib("triq/include/triq.hrl").

-import(triq, [numtests/2]).

% Generators

latitude_float() -> ?LET({A, B}, {int(), pos_integer()}, noesis_geometry:normalize_lat(A / B * 100)).

longitude_float() -> ?LET({A, B}, {int(), pos_integer()}, noesis_geometry:normalize_lng(A / B * 100)).

path() -> ?SIZED(Size, path(Size)).

path(Size) -> [{longitude_float(), latitude_float()} || _ <- lists:seq(1, Size)].

encoded_path() -> ?LET(P, path(), noesis_polyline:encode(P)).

% Properties

prop_encode_1() ->
  numtests(200, ?FORALL(Path, path(),
    is_binary(noesis_polyline:encode(Path)))).

prop_encode_2() ->
  ?FORALL(Path, path(),
    begin
      EncodedPath = noesis_polyline:encode(Path),
      length(Path) =:= length(noesis_polyline:decode(EncodedPath))
    end).

prop_decode_1() ->
  numtests(200, ?FORALL(Binary, encoded_path(),
    is_list(noesis_polyline:decode(Binary)))).

prop_decode_2() ->
  ?FORALL(Binary, encoded_path(),
    begin
      Path = noesis_polyline:decode(Binary),
      Binary =:= noesis_polyline:encode(Path)
    end).
