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
% @doc The `noesis_task' module provides some very basic `async'/`await' functionality.

-module(noesis_task).

% Types

-type ref() :: {reference(), reference() | unknown, pid() | unknown}.

-type task() :: ref() | reference().

-export_type([
  ref/0,
  task/0
]).

% API
-export([
  async/1,
  async/3,
  await/1,
  await/2,
  await_multi/1,
  await_multi/2
]).

% API

% @doc Starts a task that can be awaited on.<br />
%      This function spawns a process that is monitored by the caller process.
-spec async(function()) -> ref().
async(Fun) ->
  Parent = self(),
  Ref = make_ref(),
  {Pid, MRef} = spawn_monitor(fun() ->
    Value = Fun(),
    Parent ! {Ref, Value}
  end),
  {Ref, MRef, Pid}.

% @doc Starts a task that can be awaited on.<br />
%      This function wraps an `erlang:apply/3' in a function and delegates to {@link async/1}.
-spec async(module(), atom(), [term()]) -> ref().
async(Mod, Fun, Args) ->
  async(fun() -> apply(Mod, Fun, Args) end).

% @doc Awaits a {@link task()} reply. The `Timeout' is set to `5000' ms.
-spec await(task()) -> term().
await(Task) ->
  await(Task, 5000).

% @doc Awaits a {@link task()} reply. This function returns `{error, timeout}' after `Timeout' ms.
-spec await(task(), pos_integer()) -> term().
await(Ref, Timeout) when is_reference(Ref) ->
  await({Ref, unknown, unknown}, Timeout);
await({Ref, MRef, Pid}, Timeout) ->
  receive
    {Ref, Result} -> Result;
    {'DOWN', MRef, process, Pid, Reason} -> {error, Reason}
  after Timeout ->
    {error, timeout}
  end.

% @doc Awaits multiple {@link task()} replies. The `Timeout' per task is set to `5000' ms.
-spec await_multi(noesis_proplists:proplist(A, task())) -> noesis_proplists:proplist(A, term()).
await_multi(Tasks) ->
  await_multi(Tasks, 5000).

% @doc Awaits multiple {@link task()} replies. The `Timeout' is applied on a per task basis.
-spec await_multi(noesis_proplists:proplist(A, task()), pos_integer()) -> noesis_proplists:proplist(A, term()).
await_multi(Tasks, Timeout) ->
  await_multi_acc(Tasks, Timeout, []).

% Private

-spec await_multi_acc(noesis_proplists:proplist(A, task()), pos_integer(), noesis_proplists:proplist(A, term())) -> noesis_proplists:proplist(A, term()).
await_multi_acc([], _Timeout, Acc) ->
  Acc;
await_multi_acc([{Key, Task}|Rest], Timeout, Acc) ->
  Result = await(Task, Timeout),
  await_multi_acc(Rest, Timeout, [{Key, Result} | Acc]).
