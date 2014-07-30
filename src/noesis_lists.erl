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
% @doc The `noesis_lists' module provides functions for working with lists.

-module(noesis_lists).

% API
-export([
  group_by/2,
  pmap/2,
  split/2
]).

% API

% @doc Groups a list using a user-supplied function.
-spec group_by(fun((A) -> B), [A]) -> noesis_proplists:proplist(B, list(A)).
group_by(_Fun, []) ->
  [];
group_by(Fun, List) ->
  Dict = lists:foldl(fun({Key, Value}, Acc) ->
    dict:append(Key, Value, Acc)
  end, dict:new(), [{Fun(X), X} || X <- List]),
  dict:to_list(Dict).

% @doc Takes a function from `A's to `B's and a list of `A's and produces a list of `B's by applying the function
%      to every element in the list in parallel. The function is used to obtain the return values.<br /><br />
%      Partially based on <a href="http://erlang.org/pipermail/erlang-questions/2006-June/020834.html" target="_blank">Erlang on the Niagara</a>
%      by Joe Armstrong.
-spec pmap(fun((A) -> B), [A]) -> [B].
pmap(_Fun, []) ->
  [];
pmap(Fun, List) ->
  Ref = make_ref(),
  Schedulers = erlang:system_info(schedulers),
  {Chunks, Rest} = split(Schedulers, List),
  Index = pmap_run(Fun, Ref, 1, Chunks),
  pmap_maybe_run_and_gather(Fun, Ref, Rest, Index, length(List), 0, []).

% @doc Splits `List' into `ListA' and `ListB'. `ListA' contains the first `N' elements and `ListB' the rest of the elements (the `N'th tail).
%      If `N >= length(List)' this function will not throw an error (as opposed to `lists:split/2').<br /><br />
%      <strong>Examples</strong><br />
%      `split(2, [])' => `{[], []}'<br />
%      `split(2, [demo])' => `{[demo], []}'<br />
%      `split(2, [a, b, c, d, e])' => `{[a, b], [c, d, e]}'
-spec split(non_neg_integer(), list()) -> {ListA :: list(), ListB :: list()}.
split(_N, []) ->
  {[], []};
split(N, List) when N =< length(List) ->
  lists:split(N, List);
split(_N, List) ->
  {List, []}.

% Private

-spec pmap_run(fun(), reference(), pos_integer(), list()) -> pos_integer().
pmap_run(Fun, Ref, StartIndex, List) ->
  Parent = self(),
  lists:foldl(fun(Item, Index) ->
    _Pid = spawn(fun() ->
      Value = (catch Fun(Item)),
      Parent ! {Index, Ref, Value}
    end),
    Index + 1
  end, StartIndex, List).

-spec pmap_maybe_run_and_gather(fun(), reference(), list(), pos_integer(), non_neg_integer(), non_neg_integer(), list()) -> list().
pmap_maybe_run_and_gather(_Fun, _Ref, [], _Index, ResultLength, AccLength, Acc) when ResultLength =:= AccLength ->
  Acc2 = lists:keysort(1, Acc),
  {_Key, List} = lists:unzip(Acc2),
  List;
pmap_maybe_run_and_gather(Fun, Ref, [], Index, ResultLength, AccLength, Acc) ->
  receive
    {ResultIndex, Ref, Value} ->
      AccLength2 = AccLength + 1,
      Acc2 = [{ResultIndex, Value} | Acc],
      pmap_maybe_run_and_gather(Fun, Ref, [], Index, ResultLength, AccLength2, Acc2)
  end;
pmap_maybe_run_and_gather(Fun, Ref, [Chunk|Rest], Index, ResultLength, AccLength, Acc) ->
  receive
    {ResultIndex, Ref, Value} ->
      Index2 = pmap_run(Fun, Ref, Index, [Chunk]),
      AccLength2 = AccLength + 1,
      Acc2 = [{ResultIndex, Value} | Acc],
      pmap_maybe_run_and_gather(Fun, Ref, Rest, Index2, ResultLength, AccLength2, Acc2)
  end.
