%% Feel free to use, reuse and abuse the code in this file.

-module(shards_bench).

-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/0, stop/1]).

%% API.

start() ->
  application:ensure_all_started(shards_bench).

stop() ->
  application:stop(shards_bench).

start(_Type, _Args) ->
  % get options
  Mod = application:get_env(?MODULE, module, shards),
  Executions = application:get_env(?MODULE, n_executions, [[insert]]),
  Rand = application:get_env(?MODULE, rand_keys, true),
  Opts = application:get_env(?MODULE, opts, 4),
  Concurrency = maps:get(concurrecy, Opts, 100),
  Intensity = maps:get(intensity, Opts, 100),
  Period = maps:get(period, Opts, 100),

  % init
  ok = init(),
  ok = init_shards(Mod),
  {ok, Pid} = shards_bench_server:start(),
  monitor(process, Pid),

  % Time 0
  T0 = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

  % Logger timer
  Timeout = application:get_env(?MODULE, log_time, 1000),
  timer:apply_interval(Timeout, erlang, apply, [fun log_time/2, [Mod, T0]]),

  % launch parallel execution
  lists:foreach(fun(OpsPerExec) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    pmap(Mod, OpsPerExec, Rand, Now, Period, Intensity, Concurrency)
  end, Executions),

  % finish
  shards_bench_server:stop(),
  timer:sleep(1000),
  io:format("\e[32m ----- FINISHED!! ----- \e[0m"),
  ok.

stop(_State) ->
  ok.

%% Internals

init() ->
  _ = ets:new(shards_bench_log, [
    named_table,
    public,
    ordered_set,
    {write_concurrency, true},
    {read_concurrency, true}
  ]),
  ok.

init_shards(ets) ->
  Opts = application:get_env(?MODULE, opts, []),
  TabOpts = maps:get(tab_opts, Opts, [public, named_table]),
  _ = ets:new(?MODULE, TabOpts),
  ok;
init_shards(shards) ->
  Opts = application:get_env(?MODULE, opts, []),
  PoolSize = maps:get(pool_size, Opts, 4),
  TabOpts = maps:get(tab_opts, Opts, []),
  _ = shards:new(?MODULE, TabOpts, PoolSize),
  ok;
init_shards(mnesia) ->
  mnesia:stop(),
  mnesia:start(),
  Opts = application:get_env(?MODULE, opts, []),
  PoolSize = maps:get(pool_size, Opts, 4),
  TabOpts = [
    {frag_properties, [
      {node_pool, [node()]},
      {n_fragments, PoolSize}
    ]},
    {attributes, [key, value]}
  ],
  {atomic, ok} = mnesia:create_table(?MODULE, TabOpts),
  ok;
init_shards(_) ->
  throw(unsupported_module).

pmap(Mod, Ops, Rand, T0, Period, Intensity, N) ->
  Tasks = lists:foldl(fun(_, Acc) ->
    AsyncTask = shards_task:async(fun() ->
      task(Mod, Ops, Rand, T0, Period, Intensity)
    end), [AsyncTask | Acc]
  end, [], lists:seq(1, N)),
  lager:emergency("Max processes reached: ~p.", [N]),
  lists:foreach(fun(Task) ->
    shards_task:await(Task, infinity)
  end, Tasks).

task(Mod, Ops, Rand, T0, Period, infinity) ->
  do_task(Mod, Ops, Rand, T0, Period, 1000),
  task(Mod, Ops, Rand, T0, Period, infinity);
task(Mod, Ops, Rand, T0, Period, Intensity) ->
  do_task(Mod, Ops, Rand, T0, Period, Intensity).

do_task(_, _, _, _, _, 0) ->
  ok;
do_task(Mod, Ops, Rand, T0, Period, I) when is_integer(I) ->
  Key = case Rand of
    true -> erlang:phash2({self(), os:timestamp()});
    _    -> I
  end,
  lists:foreach(fun(Op) ->
    {T, _} = timer:tc(fun() -> do(Mod, Op, Key) end),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    shards_bench_server:log_time(Mod, Op, Now - T0, T)
  end, Ops),
  timer:sleep(Period),
  do_task(Mod, Ops, Rand, T0, Period, I - 1).

do(mnesia, insert, I) ->
  Fun = fun(Rec) -> mnesia:write(Rec) end,
  mnesia:activity(transaction, Fun, [{?MODULE, I, I}], mnesia_frag);
do(mnesia, lookup, I) ->
  Fun = fun(Key) -> mnesia:read(?MODULE, Key) end,
  mnesia:activity(transaction, Fun, [I], mnesia_frag);
do(Mod, insert, I) ->
  Mod:insert(?MODULE, {I, I});
do(Mod, lookup, I) ->
  Mod:lookup(?MODULE, I);
do(_, _, _) ->
  throw(unsupported_operation).

log_time(Mod, T0) ->
  L = ets:tab2list(shards_bench_log),
  E1 = {total, Total} = lists:keyfind(total, 1, L),
  E2 = {sum, Sum} = lists:keyfind(sum, 1, L),
  L1 = lists:delete(E2, lists:delete(E1, L)),
  Latency = Sum div Total,
  {_, ValuesL} = lists:unzip(L1),
  Throughput = round(lists:sum(ValuesL) / length(ValuesL)),
  Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  %ct:print(
  %  "\e[36m [~p] Latency = ~p us, Throughput = ~p req/sec ~n \e[0m",
  %  [Mod, MeanLatency, MeanThroughput]),
  RunQueue = statistics(run_queue),
  lager:critical("~p ~p ~p ~p ~p", [Mod, (Now - T0), RunQueue, Latency, Throughput]),
  ok.
