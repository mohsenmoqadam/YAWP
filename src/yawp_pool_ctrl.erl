-module(yawp_pool_ctrl).
-author('MohsenMoqadam@yahoo.com').

-behaviour(gen_server).

%% API
-export([start_link/1,
         get_pool_size/1,
         increment_pool_size/2,
         decrement_pool_size/2]).

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

-record(feedback_state, {last_pong_arival_time           :: non_neg_integer(),
                         last_queue_len                  :: non_neg_integer(),
                         last_commited_tasks             :: non_neg_integer(),
                         last_back_to_back_pong_interval :: non_neg_integer(),
                         terminating = disabled          :: disabled | enabled
                        }).

-record(state, {pool                    :: term(),
                pool_name               :: atom(),
                pool_max_size           :: non_neg_integer(),
                pool_min_size           :: non_neg_integer(),
                pool_current_size       :: non_neg_integer(),
                pool_decrement_timer    :: reference(),
                pool_decrement_size = 0 :: non_neg_integer(),
                workers_pid             :: list(pid()),
                ctrl_sui                :: non_neg_integer(), %% SUI: Shaper Update Interval
                ctrl_cpd                :: yawp_pool_shaper_cpd(),
                feedbacks               :: term(),
                mntr_name               :: atom(),
                beacon_name             :: atom(),
                decrement_timeout       :: non_neg_integer()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#yawp_pool{name = Name} = Pool) ->
    gen_server:start_link({local, Name},
                          ?MODULE,
                          [Pool],
                          []).

-spec get_pool_size(yawp_pool_name()) -> {ok, yawp_pool_size()}.
get_pool_size(PoolName) ->
    gen_server:call(PoolName, get_current_pool_size).

-spec increment_pool_size(yawp_pool_name(), yawp_pool_size()) ->
    {error, unacceptable_size, yawp_pool_available_size()} |
    {error, pool_max_size_reached, yawp_pool_current_size()} |
    {ok, yawp_pool_worker_pids()}.
increment_pool_size(PoolName, Size) ->
    gen_server:call(PoolName, {increment_size, Size}).

-spec decrement_pool_size(yawp_pool_name(), yawp_pool_size()) ->
    {error, pool_in_decreasing} |
    {error, unacceptable_size, yawp_pool_current_size()} |
    {error, pool_min_size_reached} |
    {error, unacceptable_size, yawp_pool_current_size()} |
    {ok, yawp_pool_worker_pids()}.
decrement_pool_size(PoolName, Size) ->
    gen_server:call(PoolName, {decrement_size, Size}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Pool]) ->
    PoolName = Pool#yawp_pool.name,
    PoolMaxSize = Pool#yawp_pool.max_size,
    PoolMinSize = Pool#yawp_pool.min_size,
    PoolSUI = Pool#yawp_pool.sui,
    DecrementTimeout = Pool#yawp_pool.sui,

    {ok, _TimerRef} = timer:send_interval(PoolSUI, update_shaper),
    {ok, MntrName} = yawp_pool_mntr:get_name(PoolName),
    {ok, BeaconName} = yawp_pool_beacon:get_name(PoolName),

    State0 = #state{pool              = Pool,
                    pool_name         = PoolName,
                    pool_max_size     = PoolMaxSize,
                    pool_min_size     = PoolMinSize,
                    ctrl_sui          = PoolSUI,
                    mntr_name         = MntrName,
                    beacon_name       = BeaconName,
                    feedbacks         = #{},
                    decrement_timeout = DecrementTimeout},
    {ok, NewState} = pool_init(State0),

    process_flag(trap_exit, true),
    {ok, NewState}.

handle_call(get_current_pool_size,
            _From,
            #state{pool_current_size = CurrentPoolSize} = State) ->
    Reply = {ok, CurrentPoolSize},
    {reply, Reply, State};
handle_call({increment_size, RequestedSize},
            _From,
            #state{pool_max_size = MaxPoolSize,
                   pool_current_size = CurrentPoolSize} = State) ->
    AvailablePoolSize = MaxPoolSize - CurrentPoolSize,
    {Reply, NewState} =
        if
            AvailablePoolSize < RequestedSize ->
                {{error, unacceptable_size, AvailablePoolSize}, State};
            CurrentPoolSize =:= MaxPoolSize ->
                {{error, pool_max_size_reached, CurrentPoolSize}, State};
            true ->
                {ok, Pids, NewState0} = add_workers(RequestedSize, State),
                {{ok, Pids}, NewState0}
        end,
    {reply, Reply, NewState};
handle_call({decrement_size, RequestedSize},
            _From,
            #state{pool_min_size = MinPoolSize,
                   pool_current_size = CurrentPoolSize,
                   pool_decrement_size = PoolDecremnetSize,
                   decrement_timeout = DecrementTimeout} = State) ->
    AvailablePoolSize = CurrentPoolSize - RequestedSize,
    {Reply, NewState} =
        if
            PoolDecremnetSize > 0 ->
                {{error, pool_in_decreasing}, State};
            AvailablePoolSize < MinPoolSize ->
                {{error, unacceptable_size, CurrentPoolSize}, State};
            CurrentPoolSize =:= MinPoolSize ->
                {{error, pool_min_size_reached}, State};
            RequestedSize > CurrentPoolSize ->
                {{error, unacceptable_size, CurrentPoolSize}, State};
            true ->
                {ok, Pids, NewState0} = remove_workers(RequestedSize, State),
                {ok, TimerRef} = timer:send_after(DecrementTimeout,
                                                  pool_decrement_timeout),
                NewState1 = NewState0#state{pool_decrement_timer = TimerRef},
                {{ok, Pids}, NewState1}
        end,
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#yawp_signal{name = worker_service_feedback,
                         meta = WorkerFeedback,
                         sender = WorkerPid},
            #state{feedbacks = OldFeedbacks} = State) ->
    #yawp_worker_feedback{worker_ql = QL,
                          worker_ct = CT} = WorkerFeedback,

    Now = ?YAWP_NOW_MICRO(),
    NewFeedbacks =
        case maps:get(WorkerPid, OldFeedbacks, undefined) of
            undefined ->
                maps:put(WorkerPid,
                         get_feedback_state(Now, Now, QL, CT),
                         OldFeedbacks);
            #feedback_state{} = OldFeedbackState ->
                maps:put(WorkerPid,
                         get_feedback_state(Now, QL, CT, OldFeedbackState),
                         OldFeedbacks)
        end,

    NewState = State#state{feedbacks = NewFeedbacks},
    {noreply, NewState};
handle_cast(#yawp_signal{name = worker_leave_pool,
                         sender = WorkerPid},
            #state{pool_decrement_size = PoolDecremnetSize,
                   feedbacks = Feedbacks0} = State) ->
    NewState = State#state{feedbacks = maps:remove(WorkerPid, Feedbacks0),
                           pool_decrement_size = PoolDecremnetSize - 1},
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(update_shaper, #state{pool_name = PoolName,
                                  feedbacks = Feedbacks,
                                  mntr_name = MntrName} = State) ->
    {ok, CPD} = update_shaper(PoolName, MntrName, Feedbacks),
    NewState = State#state{ctrl_cpd = CPD},
    {noreply, NewState};
handle_info(pool_decrement_timeout,
            #state{pool_name = PoolName,
                   pool_decrement_timer = PoolDecrementTimer,
                   beacon_name = BeaconName,
                   pool_decrement_size = PoolDecremnetSize,
                   feedbacks = Feedbacks0} = State) ->
    timer:cancel(PoolDecrementTimer),
    NewState =
        if
            PoolDecremnetSize > 0 ->
                Fun =
                    fun(WorkerPid,
                        #feedback_state{terminating = enabled}, {PN, BN, P}) ->
                            ?YAWP_LOG_ERROR("===> Forced stoped worker: ~p", [WorkerPid]),
                            {PN, BN, P};
                       (K, V, {PN, BN, P}) ->
                            {PN, BN, maps:put(K, V, P)}
                    end,
                {_, _, Feedbacks1} = maps:fold(Fun,
                                               {PoolName, BeaconName, maps:new()},
                                               Feedbacks0),

                State#state{feedbacks = Feedbacks1,
                            pool_decrement_size = 0};
            true ->
                State
        end,
    {noreply, NewState};
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
pool_init(#state{pool_name = PoolName,
                 mntr_name = MntrName} = State0) ->
    PoolName = ets:new(PoolName, [named_table,
                                  public,
                                  {read_concurrency, true}]),

    {ok, Pids, State1} = add_workers(State0),
    {ok, CPD} = update_shaper(PoolName, MntrName, Pids),

    NewState = State1#state{ctrl_cpd = CPD},
    {ok, NewState}.

update_shaper(PoolName, MntrName, PongsOrPids) when is_list(PongsOrPids);
                                                    is_map(PongsOrPids) ->
    {ok, CPD} = get_cpd(PongsOrPids),
    update_shaper(PoolName, MntrName, CPD);
update_shaper(PoolName, MntrName, CPD) ->
    %% CPD is Array.
    ShaperInfo = #yawp_shaper_info{cpd = CPD,
                                   mntr_name = MntrName},
    true = ets:insert(PoolName, ShaperInfo),
    {ok, CPD}.

add_workers(#state{pool = Pool,
                   pool_min_size = PoolMinSize,
                   beacon_name = BeaconName} = State) ->
    {ok, Pids} = yawp_pool_workers_sup:start_workers(Pool),
    [begin
         JoinPoolSignal = #yawp_signal{name = worker_join_pool,
                                       sender = Pid,
                                       receivers = [BeaconName]},
         ok = yawp_utils:send_sync_signal(JoinPoolSignal)
     end || Pid <- Pids],
    NewState = State#state{pool_current_size = PoolMinSize},
    {ok, Pids, NewState}.

add_workers(RequestedSize, #state{pool = Pool,
                                  pool_current_size = CurrentPoolSize,
                                  beacon_name = BeaconName} = State) ->
    {ok, Pids} = yawp_pool_workers_sup:start_workers(Pool, RequestedSize),
    [begin
         Signal = #yawp_signal{name = worker_join_pool,
                               sender = Pid,
                               receivers = [BeaconName]
                              },
         ok = yawp_utils:send_sync_signal(Signal)
     end || Pid <- Pids],
    NewState = State#state{pool_current_size = CurrentPoolSize + length(Pids)},
    {ok, Pids, NewState}.

remove_workers(RequestedSize, #state{pool = Pool,
                                     pool_name = PoolName,
                                     mntr_name = MntrName,
                                     pool_current_size = CurrentPoolSize,
                                     beacon_name = BeaconName,
                                     feedbacks = Feedbacks0,
                                     ctrl_cpd = CPD0} = State) ->
    {_, RemovedPids, Feedbacks1} =
        array:foldr(fun(_I, {_Pidi, {_Li, _Ui}}, {0, RP, P}) ->
                            {0, RP, P};
                       (_I, {Pidi, {_Li, _Ui}}, {RS, RP, P}) ->
                            {RS - 1,
                             [Pidi | RP],
                             maps:put(Pidi,
                                      (maps:get(Pidi, P))#feedback_state{terminating = enabled},
                                      P)}
                    end,
                    {RequestedSize, [], Feedbacks0},
                    CPD0),

    [begin
         LeavePoolSignal = #yawp_signal{name = worker_leave_pool,
                                        sender = Pid,
                                        receivers = [BeaconName]},
         ok = yawp_utils:send_sync_signal(LeavePoolSignal)
     end || Pid <- RemovedPids],
    {ok, CPD} = get_cpd(Feedbacks1),
    {ok, _} = update_shaper(PoolName, MntrName, CPD),
    ok = yawp_pool_workers_sup:stop_workers(Pool, RemovedPids),
    NewState = State#state{pool_decrement_size = length(RemovedPids),
                           pool_current_size = CurrentPoolSize - length(RemovedPids),
                           feedbacks = Feedbacks1,
                           ctrl_cpd = CPD},
    {ok, RemovedPids, NewState}.

get_cpd(Feedbacks) ->
    SL = lists:sort(fun({_, Wi}, {_, Wj}) ->
                            if Wi >= Wj -> true; true -> false end
                    end, get_normilized_weights(Feedbacks)),

    {_, _, CPD} =
        lists:foldl(fun({Pidi, Wi}, {I, Sum0, Acc0}) ->
                            Sum = Wi + Sum0,
                            Acc = array:set(I, {Pidi, {Sum0, Sum}}, Acc0),
                            {I + 1, Sum, Acc}
                    end,
                    {0, 0, array:new()},
                    SL),
    {ok, CPD}.

get_normilized_weights(Pids) when is_list(Pids) ->
    Now = ?YAWP_NOW_MICRO(),
    Feedbacks = lists:foldl(fun(Pid, Acc) ->
                                    maps:put(Pid,
                                             get_feedback_state(Now, Now, 0, 0),
                                             Acc)
                            end, #{}, Pids),
    get_normilized_weights(Feedbacks);
get_normilized_weights(Feedbacks0) when is_map(Feedbacks0) ->
    Feedbacks1 =
        maps:filter(fun(_Pidi, #feedback_state{terminating = enabled}) ->
                            false;
                       (_, _) ->
                            true
                    end, Feedbacks0),
    UnNormilizedWeights
        = maps:fold(fun(Pidi,
                        #feedback_state{last_back_to_back_pong_interval = LBTBPI,
                                        last_queue_len = LQL,
                                        last_commited_tasks = LCT}, ACC) ->
                            [{Pidi, LCT / (LQL * LBTBPI)} | ACC]
                    end, [], Feedbacks1),

    SumOfUnNormilizedWeights = lists:foldl(fun({_Pidi, TWi}, Sum) ->
                                                   TWi + Sum
                                           end, 0, UnNormilizedWeights),

    {_, NormalizedWeights} =
        lists:foldl(fun({Pidi, TWi}, {S, ACC}) ->
                            {S, [{Pidi, TWi / S} | ACC]}
                    end,
                    {SumOfUnNormilizedWeights, []},
                    UnNormilizedWeights),

    NormalizedWeights.

get_feedback_state(Now,
                   LQL0,
                   LCT0,
                   #feedback_state{last_pong_arival_time = LPAT,
                                   terminating = Terminating}) ->
    LCT = if LCT0 =:= 0 -> 1; true -> LCT0 end,
    LQL = if LQL0 =:= 0 -> 1; true -> LQL0 end,
    #feedback_state{last_pong_arival_time = Now,
                    last_back_to_back_pong_interval = Now - LPAT,
                    last_queue_len = LQL,
                    last_commited_tasks = LCT,
                    terminating = Terminating};
get_feedback_state(LPAT,
                   LBTBPI,
                   LQL0,
                   LCT0) ->
    LCT = if LCT0 =:= 0 -> 1; true -> LCT0 end,
    LQL = if LQL0 =:= 0 -> 1; true -> LQL0 end,
    #feedback_state{last_pong_arival_time = LPAT,
                    last_back_to_back_pong_interval = LBTBPI,
                    last_queue_len = LQL,
                    last_commited_tasks = LCT}.
