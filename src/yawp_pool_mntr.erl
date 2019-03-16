-module(yawp_pool_mntr).

-author('MohsenMoqadam@yahoo.com').
-behaviour(gen_server).

%% API
-export([start_link/1,
         get_name/1,
         get_stats/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).

-record(state, {workers_stats = #{} :: map(),
                shaper_stats = #{}    :: map()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#yawp_pool{name = Name} = Pool) ->
    gen_server:start_link({local, mntr_name(Name)}, ?MODULE, [Pool], []).

-spec get_name(yawp_pool_name()) -> {ok, yawp_pool_mntr_name()}.
get_name(PoolName) ->
    {ok, mntr_name(PoolName)}.

-spec get_stats(yawp_pool_name()) -> ok.
get_stats(PoolName) ->
    gen_server:cast(mntr_name(PoolName), get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([_Pool]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#yawp_signal{name = worker_service_stats} = Signal,
            #state{workers_stats = OldStats} = OldState) ->
    NewStats = update_stats(Signal, OldStats),
    NewState = OldState#state{workers_stats = NewStats},
    {noreply, NewState};
handle_cast(#yawp_signal{name = shaper_service_stats,
                         meta = ShaperStats},
            #state{shaper_stats = SS0} = OldState) ->
    #yawp_shaper_stats{search_time = ST} = ShaperStats,
    SS1 = maps:put(sum, maps:get(sum, SS0, 0) + ST, SS0),
    SS2 = maps:put(cnt, maps:get(cnt, SS1, 0) + 1, SS1),
    NewState = OldState#state{shaper_stats = SS2},
    {noreply, NewState};
handle_cast(get_stats, #state{workers_stats = WorkerStats,
                              shaper_stats = ShaperStats} = State) ->
    lists:foreach(fun({WorkerPid, WS}) ->
                          Calls   = WS#yawp_worker_stats.calls,
                          Casts   = WS#yawp_worker_stats.casts,
                          Infos   = WS#yawp_worker_stats.infos,
                          OlCalls = WS#yawp_worker_stats.ol_calls,
                          OlCasts = WS#yawp_worker_stats.ol_casts,
                          OlInfos = WS#yawp_worker_stats.ol_infos,
                          ?YAWP_LOG_INFO("===> ~p: calls:~p, casts:~p, infos:~p~n" ++
                                             "ol_calls:~p, ol_casts:~p, ol_infos:~p",
                                         [WorkerPid,
                                          Calls, Casts, Infos,
                                          OlCalls, OlCasts, OlInfos])
                  end, maps:to_list(WorkerStats)),
    STM = (maps:get(sum, ShaperStats, 0) / maps:get(cnt, ShaperStats, 1)) / 1000,
    ?YAWP_LOG_INFO("===> Search time mean: ~p usec", [STM]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mntr_name(PoolName) ->
     list_to_atom("yawp_pool_" ++ atom_to_list(PoolName) ++ "_mntr").

update_stats(#yawp_signal{name = worker_service_stats,
                          sender = WorkerPid,
                          meta = WorkerStats},
             WorkersOldStats) ->
    #yawp_worker_stats{calls    = NewCalls,
                       casts    = NewCasts,
                       infos    = NewInfos,
                       ol_calls = NewOlCalls,
                       ol_casts = NewOlCasts,
                       ol_infos = NewOlInfos} = WorkerStats,
    WorkerOldStats = maps:get(WorkerPid, WorkersOldStats, #yawp_worker_stats{}),
    NewWorkerStats =
        WorkerOldStats#yawp_worker_stats{calls = NewCalls +
                                             WorkerOldStats#yawp_worker_stats.calls,
                                         casts = NewCasts +
                                             WorkerOldStats#yawp_worker_stats.casts,
                                         infos = NewInfos +
                                             WorkerOldStats#yawp_worker_stats.infos,
                                         ol_calls = NewOlCalls +
                                             WorkerOldStats#yawp_worker_stats.ol_calls,
                                         ol_casts = NewOlCasts +
                                             WorkerOldStats#yawp_worker_stats.ol_casts,
                                         ol_infos = NewOlInfos +
                                             WorkerOldStats#yawp_worker_stats.ol_infos},

    maps:put(WorkerPid, NewWorkerStats, WorkersOldStats).
