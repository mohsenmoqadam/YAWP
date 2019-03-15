-module(yawp_test).
-author('MohsenMoqadam@yahoo.com').

-export([new/1,
         load_gen/4,
         load_gen_loop/3]).

-include("yawp.hrl").

new(Name) ->
    Pool = #yawp_pool{name = Name,
                      max_size = 32,
                      min_size = 16,
                      sui =  200,
                      bi = 200,
                      worker = {yawp_gen_worker_test, start_link, [{'Ki', 'Vi'}]}},
    yawp:new(Pool).

load_gen(Generators, PoolName, Seconds, Rate) ->
    StopAt = ?YAWP_NOW_SEC() + Seconds,
    [spawn(yawp_test, load_gen_loop, [PoolName, StopAt, Rate])
     || _ <- lists:seq(1, Generators)],

    ok.

load_gen_loop(PoolName, StopAt, Rate) ->
    Stop = StopAt - ?YAWP_NOW_SEC(),
    if
        Stop > 0 ->
            Task = hi,
            yawp:do_task(PoolName, Task),
            timer:sleep(ceil(1000 * (1/Rate))),
            load_gen_loop(PoolName, StopAt, Rate);
        true ->
            ok
    end.
