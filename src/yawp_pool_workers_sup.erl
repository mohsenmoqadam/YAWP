-module(yawp_pool_workers_sup).

-author('MohsenMoqadam@yahoo.com').
-behaviour(supervisor).

%% API
-export([start_link/1,
         start_worker/1,
         start_workers/1,
         start_workers/2,
         stop_worker/2,
         stop_workers/2]).

%% Supervisor callbacks
-export([init/1]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(#yawp_pool{name = Name} = Pool) ->
    supervisor:start_link({local, sup_name(Name)},
                          ?MODULE,
                          [Pool]).

start_worker(#yawp_pool{name = Name} = Pool) ->
    supervisor:start_child(sup_name(Name), [Pool]).

start_workers(#yawp_pool{min_size = Num} = Pool) ->
    start_workers(Pool, Num).

start_workers(#yawp_pool{name = Name}, Num) ->
    Result = [supervisor:start_child(sup_name(Name), [])
              || _ <- lists:seq(1, Num)],
    Pids = lists:foldl(fun({ok, Pid}, ACC) ->
                               [Pid | ACC]
                       end, [], Result),
    {ok, Pids}.

stop_worker(Pool, Pid) when is_pid(Pid) ->
    stop_workers(Pool, [Pid]).

stop_workers(#yawp_pool{name = Name}, Pids) when is_list(Pids) ->
    SupName = sup_name(Name),
    [ok = supervisor:terminate_child(SupName, Pid) || Pid <- Pids],

    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Pool]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},

    AWorker = #{id => yawp_gen_worker,
                start => {yawp_gen_worker, start_link, [Pool]},
                restart => transient,
                shutdown => Pool#yawp_pool.decrement_timeout,
                type => worker,
                modules => [yawp_gen_worker]},

    {ok, {SupFlags, [AWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sup_name(PoolName) ->
    list_to_atom("yawp_pool_" ++ atom_to_list(PoolName) ++ "_workers_sup").
