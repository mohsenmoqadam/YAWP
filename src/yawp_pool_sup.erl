-module(yawp_pool_sup).

-author('MohsenMoqadam@yahoo.com').
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(#yawp_pool{name = Name} = Pool) ->
    SupName = sup_name(Name),
    supervisor:start_link({local, SupName},
                          ?MODULE,
                          [Pool]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Pool]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 10},

    PoolMonitor = #{id => yawp_pool_mntr,
                    start => {yawp_pool_mntr, start_link, [Pool]},
                    restart => transient,
                    shutdown => 5000,
                    type => worker,
                    modules => [yawp_pool_mntr]},

    PoolPinger = #{id => yawp_pool_pinger,
                   start => {yawp_pool_beacon, start_link, [Pool]},
                   restart => transient,
                   shutdown => 5000,
                   type => worker,
                   modules => [yawp_pool_pinger]},

    PoolController = #{id => yawp_pool_ctrl,
                       start => {yawp_pool_ctrl, start_link, [Pool]},
                       restart => transient,
                       shutdown => 5000,
                       type => worker,
                       modules => [yawp_pool_ctrl]},

    WorkerSup = #{id => yawp_pool_workers_sup,
                  start => {yawp_pool_workers_sup, start_link, [Pool]},
                  restart => transient,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [yawp_pool_workers_sup]},

    {ok, {SupFlags, [WorkerSup,
                     PoolMonitor,
                     PoolPinger,
                     PoolController]
         }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sup_name(Name) ->
    list_to_atom("yawp_pool_" ++ atom_to_list(Name) ++ "_ctrl_sup").
