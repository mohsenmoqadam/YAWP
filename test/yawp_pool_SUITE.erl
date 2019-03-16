-module(yawp_pool_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("yawp.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    ok = application:ensure_started(yawp),

    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [simple_test].

simple_test(_Config) ->
    PoolName = test_pool,
    PoolSize = 32,
    Change = 5,
    Pool = #yawp_pool{name = PoolName,
                      max_size = 64,
                      min_size = PoolSize,
                      sui =  200,
                      bi = 200,
                      worker = {yawp_gen_worker_test, start_link, [{'Ki', 'Vi'}]}},
    yawp:new(Pool),
    ct:sleep(500),

    {ok, _} = yawp:increment_pool_size(PoolName, Change),
    NewPoolSize = PoolSize + Change,
    {ok, NewPoolSize} = yawp:get_pool_size(PoolName),

    {ok, _} = yawp:decrement_pool_size(PoolName, Change),
    {ok, PoolSize} = yawp:get_pool_size(PoolName),

    ok.
