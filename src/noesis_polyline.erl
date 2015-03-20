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
% @doc The `noesis_polyline' module provides functions for working with
%      <a href="http://en.wikipedia.org/wiki/Polygonal_chain">polylines</a>.

-module(noesis_polyline).

% Types

-type line() :: binary().

-export_type([
  line/0
]).

% API
-export([
  encode/1,
  decode/1
]).

% API

% @doc Takes a {@link noesis_geometry:path()} and returns a {@link line()}.<br /><br />
%      Based on the algorithm described <a href="https://developers.google.com/maps/documentation/utilities/polylinealgorithm">here</a>.
-spec encode(noesis_geometry:path()) -> line().
encode(Path) ->
  encode_acc(Path, 0, 0, <<>>).

% @doc Takes a {@link line()} and returns a {@link noesis_geometry:path()}.<br /><br />
%      Based on the algorithm described <a href="https://developers.google.com/maps/documentation/utilities/polylinealgorithm">here</a>.
-spec decode(line()) -> noesis_geometry:path().
decode(Line) ->
  decode_acc(Line, 0, 0, []).

% Private

-spec encode_acc(noesis_geometry:path(), noesis_geometry:latitude(), noesis_geometry:longitude(), line()) -> line().
encode_acc([], _PLat, _PLng, Acc) ->
  Acc;
encode_acc([{Lng, Lat}|Rest], PLat, PLng, Acc) ->
  LatE5 = round(Lat * 1.0e5),
  LngE5 = round(Lng * 1.0e5),
  EncodedLat = encode_part(encode_sign(LatE5 - PLat), <<>>),
  EncodedLng = encode_part(encode_sign(LngE5 - PLng), <<>>),
  encode_acc(Rest, LatE5, LngE5, <<Acc/binary, EncodedLat/binary, EncodedLng/binary>>).

-spec encode_sign(integer()) -> integer().
encode_sign(Num) when Num < 0 ->
  bnot (Num bsl 1);
encode_sign(Num) ->
  Num bsl 1.

-spec encode_part(integer(), binary()) -> binary().
encode_part(Num, Result) when Num < 32 ->
  <<Result/binary, (Num + 63)>>;
encode_part(Num, Result) ->
  Value = (32 bor (Num band 31)) + 63,
  encode_part(Num bsr 5, <<Result/binary, Value>>).

-spec decode_acc(line(), noesis_geometry:latitude(), noesis_geometry:longitude(), noesis_geometry:path()) -> noesis_geometry:path().
decode_acc(<<>>, _Lat, _Lng, Acc) ->
  lists:reverse(Acc);
decode_acc(Line, Lat, Lng, Acc) ->
  {DLat, Rest} = decode_part(Line, 32, 0, 0),
  Lat2 = Lat + DLat,
  {DLng, Rest2} = decode_part(Rest, 32, 0, 0),
  Lng2 = Lng + DLng,
  decode_acc(Rest2, Lat2, Lng2, [{Lng2 / 1.0e5, Lat2 / 1.0e5} | Acc]).

-spec decode_part(line(), non_neg_integer(), non_neg_integer(), integer()) -> {integer(), line()}.
decode_part(Line, B, _Shift, Result) when B < 32 ->
  Result2 = if
    Result band 1 == 0 -> Result bsr 1;
    true -> bnot (Result bsr 1)
  end,
  {Result2, Line};
decode_part(<<C:8, Rest/binary>>, _OldB, Shift, Result) ->
  B = C - 63,
  Result2 = Result bor ((B band 31) bsl Shift),
  decode_part(Rest, B, Shift + 5, Result2).
