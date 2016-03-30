%% Feel free to use, reuse and abuse the code in this file.

-module(shards_bench_server).

-behaviour(gen_server).

%% API
-export([
  start/0,
  stop/0,
  log_time/4
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

log_time(Mod, Op, Sec, Time) ->
  gen_server:cast(?MODULE, {log_time, Mod, Op, Sec, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),

  % init log table
  true = ets:insert(shards_bench_log, {total, 0}),
  true = ets:insert(shards_bench_log, {sum, 0}),

  %Timeout = application:get_env(?MODULE, log_time, 1000),
  %timer:send_interval(Timeout, log_time),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({log_time, _Mod, _Op, Sec, Time}, State) ->
  _ = ets:update_counter(shards_bench_log, total, {2, 1}),
  _ = ets:update_counter(shards_bench_log, sum, {2, Time}),
  update_sec(Sec),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  lager:emergency("EXIT: ~p", [Info]),
  {noreply, State}.

terminate(_, State) ->
  lager:emergency("EXIT: ~p", [State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_sec(Sec) ->
  case ets:lookup(shards_bench_log, Sec) of
    [{Sec, _}] ->
      ets:update_counter(shards_bench_log, Sec, {2, 1});
    [] ->
      ets:insert(shards_bench_log, {Sec, 0})
  end.
