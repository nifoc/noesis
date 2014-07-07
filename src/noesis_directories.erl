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
% @doc The `noesis_directories' module includes functions that work with directories and paths.

-module(noesis_directories).

% API
-export([
 priv/1
]).

% API

% @doc Returns the path to `priv/' directory of a given module.
-spec priv(module()) -> string().
priv(Mod) ->
  Ebin = filename:dirname(code:which(Mod)),
  Root = filename:dirname(Ebin),
  Path = case string:substr(Root, 1, 1) of
    "." ->
      {ok, Cwd} = file:get_cwd(),
      EunitTest = string:str(Cwd, ".eunit") > 0,
      CommonTest = string:str(Cwd, "ct_run") > 0,
      if
        EunitTest -> filename:dirname(Cwd);
        CommonTest -> filename:dirname(filename:dirname(Cwd));
        true -> Root
      end;
    _ -> Root
  end,
  filename:join(Path, "priv").
