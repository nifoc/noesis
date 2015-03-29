% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(noesis_task_test).

-include_lib("eunit/include/eunit.hrl").

async_await_test() ->
  ?assertEqual(ok, noesis_task:await(noesis_task:async(fun() -> ok end))),
  ?assertEqual(3, noesis_task:await(noesis_task:async(lists, sum, [[1, 2]]))),
  ?assertEqual({error, test}, noesis_task:await(noesis_task:async(fun() -> exit(test) end))),
  {Ref, _MRef, _Pid} = noesis_task:async(fun() -> ok end),
  ?assertEqual(ok, noesis_task:await(Ref)).

await_test_() ->
  {timeout, 10, fun() ->
    Ref = make_ref(),
    ?assertEqual({error, timeout}, noesis_task:await(Ref))
  end}.
