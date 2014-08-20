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
% @doc The `noesis_proplists' module includes functions that work on property lists.

-module(noesis_proplists).

-compile({no_auto_import, [get_keys/1]}).

% Types

-type proplist() :: [{term(), term()}].
-type proplist(Key) :: [{Key, term()}].
-type proplist(Key, Value) :: [{Key, Value}].

-export_type([
  proplist/0,
  proplist/1,
  proplist/2
]).

% API
-export([
  get_keys/1,
  get_value/2,
  get_value/3,
  delete_keys/2,
  keypos/2,
  extract/2,
  extract/3,
  extract/4,
  partial_extract/2,
  partial_extract/3,
  merge/2,
  merge/3
]).

% API

% @doc Returns a list of all keys in the property list, not including duplicates.
-spec get_keys(proplist()) -> [term()].
get_keys(List) ->
  Keys = [Key || {Key, _Value} <- List],
  lists:usort(Keys).

% @doc Delegates to {@link get_value/3} and sets the default return value to `undefined'.
-spec get_value(term(), proplist()) -> term().
get_value(Key, List) ->
  get_value(Key, List, undefined).

% @doc Returns the value of a key/value property in `List'. Returns the default value if `Key' is not found.
-spec get_value(term(), proplist(), term()) -> term().
get_value(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false -> Default;
    {Key, Value} -> Value
  end.

% @doc Removes multiple keys from a property list and returns a new one. Duplicate keys are also removed.
-spec delete_keys([term()], proplist()) -> proplist().
delete_keys([], List) ->
  List;
delete_keys([Key|Rest]=Keys, List) ->
  List2 = lists:keydelete(Key, 1, List),
  case lists:keymember(Key, 1, List2) of
    true -> delete_keys(Keys, List2);
    false -> delete_keys(Rest, List2)
  end.

% @doc Returns the position of a key in a property list. If `Key' does not exist `undefined' will be returned.
-spec keypos(term(), proplist()) -> pos_integer() | undefined.
keypos(Key, List) ->
  keypos_acc(Key, List, 1).

% @doc Delegates to {@link extract/4}. `NullValue' is set to `undefined' and `Defaults' is set to an empty list.
-spec extract([term()], proplist()) -> proplist().
extract(Keys, List) ->
  extract(Keys, List, undefined, []).

% @doc Delegates to {@link extract/4}. Allows you to set `NullValue' and sets `Defaults' to an empty list.
-spec extract([term()], proplist(), term()) -> proplist().
extract(Keys, List, NullValue) ->
  extract(Keys, List, NullValue, []).

% @doc Extracts key/value pairs from a property list and returns a new property list containing only extracted pairs.<br />
%      You can provide a set of default values that will be used as fallback if the key is not found in `List'. If the key is not
%      found in `Defaults' either, the `NullValue' will be used for the value part.
-spec extract([term()], proplist(), term(), proplist()) -> proplist().
extract(Keys, List, NullValue, Defaults) ->
  lists:map(fun(Key) ->
    case get_value(Key, List) of
      undefined ->
        Default = get_value(Key, Defaults, NullValue),
        {Key, Default};
      Value -> {Key, Value}
    end
  end, lists:usort(Keys)).

% @doc Delegates to {@link partial_extract/3}. `NullValue' is set to `undefined'.
-spec partial_extract([term()], proplist()) -> proplist().
partial_extract(Keys, List) ->
  partial_extract(Keys, List, undefined).

% @doc Extracts key/value pairs from a property list and returns a new property list containing only extracted pairs.<br />
%      Pairs not found in `List' (and all those matching `NullValue') will not be included in the property list that is returned.
-spec partial_extract([term()], proplist(), term()) -> proplist().
partial_extract(Keys, List, NullValue) ->
  List2 = extract(Keys, List, NullValue),
  [{Key, Value} || {Key, Value} <- List2, Value =/= NullValue].

% @doc Delegates to {@link merge/3}. `Fun' is set to function that always uses the value from `ListB'.
-spec merge(proplist(), proplist()) -> proplist().
merge(ListA, ListB) ->
  merge(fun(_Key, _ValueA, ValueB) -> ValueB end, ListA, ListB).

% @doc Merges two property lists, `ListA' and `ListB', to create a new list. All the Key/Value pairs from both lists are
%      included in the new property list. If a key occurs in both lists then `Fun' is called with the key and both values
%      to return a new value.
-spec merge(fun((Key, ValueA, ValueB) -> Value), proplist(Key, ValueA), proplist(Key, ValueB)) -> proplist(Key, Value).
merge(Fun, ListA, ListB) ->
  DictA = orddict:from_list(ListA),
  DictB = orddict:from_list(ListB),
  MergedDict = orddict:merge(Fun, DictA, DictB),
  orddict:to_list(MergedDict).

% Private

-spec keypos_acc(term(), proplist(), pos_integer()) -> pos_integer() | undefined.
keypos_acc(_Key, [], _Acc) ->
  undefined;
keypos_acc(Key, [{Key, _Value}|_Rest], Acc) ->
  Acc;
keypos_acc(Key, [_Pair|Rest], Acc) ->
  keypos_acc(Key, Rest, Acc+1).
