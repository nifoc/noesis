% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_binary_test).

-include_lib("eunit/include/eunit.hrl").

join_test() ->
  ?assertEqual(<<>>, noesis_binary:join([], <<", ">>)),
  ?assertEqual(<<",">>, noesis_binary:join([<<>>, <<>>], <<",">>)),
  ?assertEqual(<<"Hello">>, noesis_binary:join([<<"Hello">>], <<", ">>)),
  ?assertEqual(<<"Hello, World">>, noesis_binary:join([<<"Hello">>, <<"World">>], <<", ">>)),
  ?assertEqual(<<"foo bar baz">>, noesis_binary:join([<<"foo">>, <<"bar">>, <<"baz">>], <<" ">>)),
  ?assertEqual(<<"foobarbaz">>, noesis_binary:join([<<"foo">>, <<"bar">>, <<"baz">>], <<>>)),
  ?assertEqual(<<"Hello-this-is-a-long-joined-binary-list">>,
    noesis_binary:join([<<"Hello">>, <<"this">>, <<"is">>, <<"a">>, <<"long">>, <<"joined">>, <<"binary">>, <<"list">>], <<"-">>)).

to_hex_test() ->
  ?assertEqual("390a", noesis_binary:to_hex(<<1337, 10>>)).
