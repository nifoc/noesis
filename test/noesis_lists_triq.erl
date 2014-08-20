% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_lists_triq).

-include_lib("triq/include/triq.hrl").

% Generators

non_neg_int() ->
  ?SUCHTHAT(I, int(), I >= 0).

% Properties

prop_pfilter_1() ->
  ?FORALL({X, Xs}, {int(), list(int())},
    not lists:member(X, noesis_lists:pfilter(fun(Y) -> Y =/= X end, Xs))).

prop_pfilter_2() ->
  ?FORALL({X, Xs}, {int(), list(int())},
    begin
      Fun = fun(Y) -> Y =/= X end,
      ListA = noesis_lists:pfilter(Fun, Xs),
      ListB = lists:filter(Fun, Xs),
      ListA =:= ListB
    end).

prop_pmap_1() ->
  ?FORALL(List, list(int()),
    begin
      Fun = fun(X) -> X + 1 end,
      ListA = noesis_lists:pmap(Fun, List),
      ListB = lists:map(Fun, List),
      ListA =:= ListB
    end).

prop_split_1() ->
  ?FORALL({N, List}, {non_neg_int(), list(int())},
    length(element(1, noesis_lists:split(N, List))) =< N).

prop_split_2() ->
  ?FORALL({N, List}, {non_neg_int(), list(int())},
    ?IMPLIES(length(List) >= N,
      length(element(1, noesis_lists:split(N, List))) =:= N)).

prop_split_3() ->
  ?FORALL({N, List}, {non_neg_int(), list(int())},
    ?IMPLIES(length(List) < N,
      length(element(2, noesis_lists:split(N, List))) =:= 0)).
