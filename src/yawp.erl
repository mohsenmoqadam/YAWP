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

new(Pool = #yawp_pool{}) ->
    yawp_sup:new_pool(Pool).

get_pool_size(PoolName) ->
    yawp_pool_ctrl:get_pool_size(PoolName).

get_pool_stats(PoolName) ->
    yawp_pool_mntr:get_stats(PoolName).

increment_pool_size(PoolName, Size) ->
    yawp_pool_ctrl:increment_pool_size(PoolName, Size).

decrement_pool_size(PoolName, Size) ->
    yawp_pool_ctrl:decrement_pool_size(PoolName, Size).

do_task(PoolName, Task) ->
    {ok, WorkerPid} = get_worker(PoolName),
    gen_server:cast(WorkerPid, Task).

do_sync_task(PoolName, Task) ->
    {ok, WorkerPid} = get_worker(PoolName),
    gen_server:call(WorkerPid, Task).

-ifdef(PROFILE_DEV).
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

get_worker(Array, Key) ->
    get_worker(Array, Key, 0, array:size(Array)).

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
