%% Feel free to use, reuse and abuse the code in this file.

-module(shards_bench_log_processor).

%% API
-export([parse/1]).

parse(FileName) ->
  {ok, IoDevice} = file:open(FileName, [read]),
  for_each_line(IoDevice, []).

for_each_line(IoDevice, Acc) ->
  %io:get_line(IO, '')
  case file:read_line(IoDevice) of
    {ok, Line} ->
      for_each_line(IoDevice, parse_line(Line, Acc));
    eof ->
      file:close(IoDevice),
      lists:reverse(Acc)
  end.

parse_line(Line, Acc) ->
  Line1 = re:replace(Line, "\n", "", [{return, list}]),
  L = string:tokens(Line1, " "),
  io:format("Line: ~p~n", [L]),
  [lists:last(L) | Acc].
