% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_lists_test).

-include_lib("eunit/include/eunit.hrl").

group_by_test() ->
  ?assertEqual([{x, [1, 2, 3]}], noesis_lists:group_by(fun(_V) -> x end, [1, 2, 3])),
  ?assertEqual([{x, [{x, 1}, {x, 2}, {x, 3}]}], noesis_lists:group_by(fun({K, _V}) -> K end, [{x, 1}, {x, 2}, {x, 3}])),
  ?assertEqual([{x, [{x, 1}, {x, 3}]}, {y, [{y, 2}]}], noesis_lists:group_by(fun({K, _V}) -> K end, [{x, 1}, {y, 2}, {x, 3}])),
  ?assertEqual([], noesis_lists:group_by(fun(_V) -> x end, [])).

pfilter_test() ->
  FunA = fun(X) -> X rem 2 =:= 0 end,
  ListA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 66, 99, 6, 3, 9000],
  ?assertEqual(lists:filter(FunA, ListA), noesis_lists:pfilter(FunA, ListA)),
  ?assertEqual(lists:sum(lists:filter(FunA, ListA)), lists:sum(noesis_lists:pfilter(FunA, ListA))),
  ?assertEqual(lists:sum(noesis_lists:pfilter(FunA, ListA)), lists:sum(noesis_lists:pfilter(FunA, ListA, [{retain_order, false}]))),
  ?assertEqual([], noesis_lists:pfilter(FunA, [])),
  ?assertEqual(lists:filter(FunA, ListA), noesis_lists:pfilter(FunA, ListA, [{parallelism, 1}])),
  ?assertEqual(lists:filter(FunA, ListA), noesis_lists:pfilter(FunA, ListA, [{parallelism, 2}])).

pmap_test() ->
  FunA = fun(X) -> X + 1 end,
  ListA = [1, 2, 10, 66, 99, 6, 3, 9000],
  ?assertEqual(lists:map(FunA, ListA), noesis_lists:pmap(FunA, ListA)),
  ?assertEqual(lists:sum(lists:map(FunA, ListA)), lists:sum(noesis_lists:pmap(FunA, ListA))),
  ?assertEqual(lists:sum(noesis_lists:pmap(FunA, ListA)), lists:sum(noesis_lists:pmap(FunA, ListA, [{retain_order, false}]))),
  ?assertEqual(lists:map(FunA, ListA), noesis_lists:pmap(FunA, ListA, [{parallelism, 1}])),
  ?assertEqual(lists:map(FunA, ListA), noesis_lists:pmap(FunA, ListA, [{parallelism, 2}])),
  FunB = fun({_K, V}) -> V end,
  ListB = [{x, <<"hello">>}, {y, <<"test">>}, {z, <<"foo">>}],
  ?assertEqual(lists:map(FunB, ListB), noesis_lists:pmap(FunB, ListB)),
  ?assertEqual([], noesis_lists:pmap(FunB, [])).

split_test() ->
  List = [1, 2, 3, 4, 5, 6, 7, 8, 9],
  ?assertEqual({List, []}, noesis_lists:split(10, List)),
  ?assertEqual({[1], [2, 3, 4, 5, 6, 7, 8, 9]}, noesis_lists:split(1, List)),
  ?assertEqual({[], []}, noesis_lists:split(10, [])),
  ?assertEqual({[1, 2, 3], [4, 5, 6, 7, 8, 9]}, noesis_lists:split(3, List)),
  ?assertEqual({List, []}, noesis_lists:split(100, List)).
