-module(yawp).
-author('MohsenMoqadam@yahoo.com').

-export([new/1,
         do_task/2,
         do_sync_task/2,
         increment_pool_size/2,
         decrement_pool_size/2,
         get_pool_size/1,
         get_pool_stats/1
        ]).

-include("yawp.hrl").

-spec new(yawp_poo()) -> yawp_pool_sup().
new(Pool = #yawp_pool{}) ->
    yawp_sup:new_pool(Pool).

-spec get_pool_size(yawp_pool_name()) -> {ok, yawp_pool_size()}.
get_pool_size(PoolName) ->
    yawp_pool_ctrl:get_pool_size(PoolName).

-spec get_pool_stats(yawp_pool_name()) -> ok.
get_pool_stats(PoolName) ->
    yawp_pool_mntr:get_stats(PoolName),

    ok.

-spec increment_pool_size(yawp_pool_name(), yawp_pool_size()) ->
    {error, unacceptable_size, yawp_pool_available_size()} |
    {error, pool_max_size_reached, yawp_pool_current_size()} |
    {ok, yawp_pool_worker_pids()}.
increment_pool_size(PoolName, Size) ->
    yawp_pool_ctrl:increment_pool_size(PoolName, Size).

-spec decrement_pool_size(yawp_pool_name(), yawp_pool_size()) ->
    {error, pool_in_decreasing} |
    {error, unacceptable_size, yawp_pool_current_size()} |
    {error, pool_min_size_reached} |
    {error, unacceptable_size, yawp_pool_current_size()} |
    {ok, yawp_pool_worker_pids()}.
decrement_pool_size(PoolName, Size) ->
    yawp_pool_ctrl:decrement_pool_size(PoolName, Size).

-spec do_task(yawp_pool_name(), term()) -> ok.
do_task(PoolName, Task) ->
    {ok, WorkerPid} = get_worker(PoolName),
    gen_server:cast(WorkerPid, Task).

-spec do_sync_task(yawp_pool_name(), term()) -> {ok, term()}.
do_sync_task(PoolName, Task) ->
    {ok, WorkerPid} = get_worker(PoolName),
    gen_server:call(WorkerPid, Task).

-ifdef(PROFILE_DEV).
-spec get_worker(yawp_pool_name()) -> {ok, yawp_pool_worker_pid()}.
get_worker(PoolName) ->
    T1 = ?YAWP_NOW_NANO(),
    [#yawp_shaper_info{cpd = CPD,
                       mntr_name = MntrName}] =
        ets:lookup(PoolName, yawp_shaper_info),
    {ok, WorkerPid} = get_worker(CPD, rand:uniform()),

    ShaperStats = #yawp_shaper_stats{search_time = ?YAWP_NOW_NANO() - T1},
    Signal = #yawp_signal{name = shaper_service_stats,
                          meta = ShaperStats,
                          sender = self(),
                          receivers = [MntrName]},
    ok = yawp_utils:send_signal(Signal),

    {ok, WorkerPid}.
-else.
get_worker(PoolName) ->
    [#yawp_shaper_info{cpd = CPD}] = ets:lookup(PoolName, yawp_shaper_info),
    {ok, WorkerPid} = get_worker(CPD, rand:uniform()),
    {ok, WorkerPid}.
-endif.

-spec get_worker(yawp_pool_shaper_cpd(),
                 yawp_pool_shaper_cpd_prob()) ->
    yawp_pool_worker_pid().
get_worker(Array, Key) ->
    get_worker(Array, Key, 0, array:size(Array)).

-spec get_worker(yawp_pool_shaper_cpd(),
                 yawp_pool_shaper_cpd_prob(),
                 yawp_pool_shaper_cpd_boundary(),
                 yawp_pool_shaper_cpd_boundary()) ->
                        {ok, yawp_pool_worker_pid()}.
get_worker(_, _, Low, High) when Low > High -> error;
get_worker(Array, Key, Low, High) ->
    Mid = (Low + High) div 2,
    {Pidi, {From, To}} = array:get(Mid, Array),
    if
        Key > To ->
            get_worker(Array, Key, Mid + 1, High);
        Key < From ->
            get_worker(Array, Key, Low, Mid - 1);
        true ->
            {ok, Pidi}
    end.
