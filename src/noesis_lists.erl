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
% @doc The `noesis_lists' module provides functions for working with lists.

-module(noesis_lists).

% API
-export([
  group_by/2,
  pfilter/2,
  pfilter/3,
  pmap/2,
  pmap/3,
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

% @doc Delegates to {@link pfilter/3} and uses default options.<br /><br />
%      <strong>Default Options</strong><br />
%      <pre><code>[{retain_order, true}, {parallelism, default_parallelism()}]</code></pre>
-spec pfilter(fun((T) -> boolean()), [T]) -> [T].
pfilter(Fun, List) ->
  pfilter(Fun, List, []).

% @doc Returns a list of all elements in `List' for which `Fun' returns `true'. Applying `Fun' to every element in the list
%      happens in parallel.
-spec pfilter(fun((T) -> boolean()), [T], noesis_proplists:proplist(atom(), term())) -> [T].
pfilter(_Fun, [], _Options) ->
  [];
pfilter(Fun, List, Options) ->
  case noesis_proplists:get_value(parallelism, Options, default_parallelism()) of
    1 -> lists:filter(Fun, List);
    Parallelism ->
      List2 = parallel_init(Fun, Parallelism, List),
      pfilter_extract_results(List2, Options)
  end.

% @doc Delegates to {@link pmap/3} and uses default options.<br /><br />
%      <strong>Default Options</strong><br />
%      <pre><code>[{retain_order, true}, {parallelism, default_parallelism()}]</code></pre>
-spec pmap(fun((A) -> B), [A]) -> [B].
pmap(Fun, List) ->
  pmap(Fun, List, []).

% @doc Takes a function from `A's to `B's and a list of `A's and produces a list of `B's by applying the function
%      to every element in the list in parallel. The function is used to obtain the return values.<br /><br />
%      Partially based on <a href="http://erlang.org/pipermail/erlang-questions/2006-June/020834.html" target="_blank">Erlang on the Niagara</a>
%      by Joe Armstrong.
-spec pmap(fun((A) -> B), [A], noesis_proplists:proplist(atom(), term())) -> [B].
pmap(_Fun, [], _Options) ->
  [];
pmap(Fun, List, Options) ->
  case noesis_proplists:get_value(parallelism, Options, default_parallelism()) of
    1 -> lists:map(Fun, List);
    Parallelism ->
      List2 = parallel_init(Fun, Parallelism, List),
      pmap_extract_results(List2, Options)
  end.

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

-spec default_parallelism() -> pos_integer().
default_parallelism() -> round(erlang:system_info(schedulers) * 1.5).

-spec parallel_init(fun(), pos_integer(), list()) -> [{pos_integer(), term(), term()}].
parallel_init(Fun, Parallelism, List) ->
  Ref = make_ref(),
  {Chunks, Rest} = split(Parallelism, List),
  Index = parallel_run(Fun, Ref, 1, Chunks),
  parallel_run_and_gather(Fun, Ref, Rest, length(List), Index, [], 0).

-spec parallel_run(fun(), reference(), pos_integer(), list()) -> pos_integer().
parallel_run(Fun, Ref, StartIndex, List) ->
  Parent = self(),
  lists:foldl(fun(Item, Index) ->
    _Pid = spawn(fun() ->
      Value = (catch Fun(Item)),
      Parent ! {Index, Ref, Item, Value}
    end),
    Index + 1
  end, StartIndex, List).

-spec parallel_run_and_gather(fun(), reference(), list(), non_neg_integer(), pos_integer(), list(), non_neg_integer()) -> [{pos_integer(), term(), term()}].
parallel_run_and_gather(_Fun, _Ref, [], ResultLength, _Index, Acc, AccLength) when ResultLength =:= AccLength ->
  Acc;
parallel_run_and_gather(Fun, Ref, [], ResultLength, Index, Acc, AccLength) ->
  receive
    {ResultIndex, Ref, Item, Value} ->
      Acc2 = [{ResultIndex, Item, Value} | Acc],
      AccLength2 = AccLength + 1,
      parallel_run_and_gather(Fun, Ref, [], ResultLength, Index, Acc2, AccLength2)
  end;
parallel_run_and_gather(Fun, Ref, [Chunk|Rest], ResultLength, Index, Acc, AccLength) ->
  receive
    {ResultIndex, Ref, Item, Value} ->
      Index2 = parallel_run(Fun, Ref, Index, [Chunk]),
      Acc2 = [{ResultIndex, Item, Value} | Acc],
      AccLength2 = AccLength + 1,
      parallel_run_and_gather(Fun, Ref, Rest, ResultLength, Index2, Acc2, AccLength2)
  end.

-spec pmap_extract_results([{pos_integer(), term(), A}], noesis_proplists:proplist(atom(), term())) -> [A].
pmap_extract_results(List, Options) ->
  List2 = case noesis_proplists:get_value(retain_order, Options, true) of
    true -> lists:keysort(1, List);
    false -> List
  end,
  {_Keys, _Items, Values} = lists:unzip3(List2),
  Values.

-spec pfilter_extract_results([{pos_integer(), T, boolean()}], noesis_proplists:proplist(atom(), term())) -> [T].
pfilter_extract_results(List, Options) ->
  List2 = case noesis_proplists:get_value(retain_order, Options, true) of
    true -> lists:keysort(1, List);
    false -> List
  end,
  [Item || {_Key, Item, Value} <- List2, Value == true].
