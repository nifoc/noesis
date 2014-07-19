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
% @doc The `noesis_binary' module provides functions for working with binaries.

-module(noesis_binary).

% API
-export([
  join/2,
  to_hex/1
]).

% API

% @doc Joins a list of binaries together using `Sep'.
-spec join([binary()], binary()) -> binary().
join([], _Sep) ->
  <<>>;
join([Part], _Sep) ->
  Part;
join([Head|Tail], Sep) ->
  lists:foldl(fun(Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end, Head, Tail).

% @doc Converts a binary to a hexadecimal string.
-spec to_hex(binary()) -> noesis_string:hexstring().
to_hex(Binary) ->
  Int = binary:decode_unsigned(Binary),
  string:to_lower(integer_to_list(Int, 16)).
