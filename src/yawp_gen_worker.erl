-module(yawp_gen_worker).

-author('MohsenMoqadam@yahoo.com').
-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/3,
         start_link/4,
         call/2,
         call/3,
         cast/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-export([behaviour_info/1]).

-include("yawp.hrl").

-record(behaviour_state,
        {worker_state  :: term(),
         worker_module :: atom(),
         pool_name     :: atom(),
         pool_mntr     :: atom(),
         worker_stats  = #yawp_worker_stats{} :: yawp_worker_stats()}).

%%%===================================================================
%%% API
%%%===================================================================

behaviour_info(callbacks) ->
    [{start_link, 1},
     {init, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2},
     {code_change, 3},
     {format_status, 2}];
behaviour_info(_) ->
    undefined.

start_link(#yawp_pool{name = Name,
                      worker = {M, F, A}}) ->
    erlang:apply(M, F, [[{pool_name, Name}] ++ A]).

start_link(Module, Pool, Options) ->
    gen_server:start_link(?MODULE,
                          [Module, Pool],
                          Options).

start_link(Name, Module, Pool, Options) ->
    gen_server:start_link(Name,
                          ?MODULE,
                          [Module, Pool],
                          Options).

call(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).

call(ServerRef, Request, Timeout) ->
    gen_server:call(ServerRef, Request, Timeout).

cast(ServerRef, Request) ->
    gen_server:cast(ServerRef, Request).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WorkerModule, [{pool_name, PoolName}, A]]) ->
    {ok, PoolMntr} = yawp_pool_mntr:get_name(PoolName),

    case WorkerModule:init(PoolName, A) of
        {ok, WorkerState} ->
            BehaviourState =
                #behaviour_state{worker_state = WorkerState,
                                 worker_module = WorkerModule,
                                 pool_name = PoolName,
                                 pool_mntr = PoolMntr},
            {ok, BehaviourState};
        {ok, WorkerState, {continue,Continue}} ->
            BehaviourState =
                #behaviour_state{worker_state = WorkerState,
                                 worker_module = WorkerModule,
                                 pool_name = PoolName,
                                 pool_mntr = PoolMntr},
            {ok, BehaviourState, {continue, Continue}};
        {ok, WorkerState, hibernate} ->
            BehaviourState =
                #behaviour_state{worker_state = WorkerState,
                                 worker_module = WorkerModule,
                                 pool_name = PoolName},
            {ok, BehaviourState, hibernate};
        {ok, WorkerState, Timeout} ->
            BehaviourState =
                #behaviour_state{worker_state = WorkerState,
                                 worker_module = WorkerModule,
                                 pool_name = PoolName,
                                 pool_mntr = PoolMntr},
            {ok, BehaviourState, Timeout};
        {stop, WorkerStopReason} ->
            BehaviourStopReason = WorkerStopReason,
            {stop, BehaviourStopReason};
        ignore ->
            ignore
    end.

handle_call(Request,
            From,
            #behaviour_state{worker_state = WorkerState,
                             worker_module = WorkerModule,
                             worker_stats = OldWorkerStats} =
                BehaviourState) ->
    NewWorkerStats = calc_stats(call, OldWorkerStats),
    case WorkerModule:handle_call(Request, From, WorkerState) of
        {reply, WorkerReply, NewWorkerState} ->
            BehaviourReply = WorkerReply,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {reply, BehaviourReply, NewBehaviourState};
        {reply, WorkerReply, NewWorkerState, {continue, Continue}} ->
            BehaviourReply = WorkerReply,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {reply, BehaviourReply, NewBehaviourState, {continue, Continue}};
        {reply, WorkerReply, NewWorkerState, hibernate} ->
            BehaviourReply = WorkerReply,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {reply, BehaviourReply, NewBehaviourState, hibernate};
        {reply, WorkerReply, NewWorkerState, Timeout} ->
            BehaviourReply = WorkerReply,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {reply, BehaviourReply, NewBehaviourState, Timeout};
        {noreply, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState};
        {noreply, NewWorkerState, hibernate} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, hibernate};
        {noreply, NewWorkerState, {continue, Continue}} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, {continue, Continue}};
        {noreply, NewWorkerState, Timeout} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, Timeout};
        {stop, WorkerStopReason, WorkerReply, NewWorkerState} ->
            BehaviourReply = WorkerReply,
            BehaviourStopReason = WorkerStopReason,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {stop, BehaviourStopReason, BehaviourReply, NewBehaviourState};
        {stop, WorkerStopReason, NewWorkerState} ->
            BehaviourStopReason = WorkerStopReason,
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {stop, BehaviourStopReason, NewBehaviourState}
    end.

handle_cast(#yawp_signal{name = beacon_service_feedback},
            #behaviour_state{pool_name = PoolName,
                             pool_mntr = PoolMntr,
                             worker_stats = WorkerStats} = State) ->
    Self = self(),

    CT =
        WorkerStats#yawp_worker_stats.calls +
        WorkerStats#yawp_worker_stats.casts +
        WorkerStats#yawp_worker_stats.infos,
    {message_queue_len, QL} = erlang:process_info(Self, message_queue_len),
    WorkerFeedback = #yawp_worker_feedback{worker_ql = QL,
                                           worker_ct = CT},
    WorkerServiceFeedback = #yawp_signal{name = worker_service_feedback,
                                         meta = WorkerFeedback,
                                         sender = self(),
                                         receivers = [PoolName]},
    yawp_utils:send_signal(WorkerServiceFeedback),

    WorkerServiceStats = #yawp_signal{name = worker_service_stats,
                                      meta = WorkerStats,
                                      sender = self(),
                                      receivers = [PoolMntr]},
    yawp_utils:send_signal(WorkerServiceStats),

    NewBehaviourState = State#behaviour_state{worker_stats = #yawp_worker_stats{}},
    {noreply, NewBehaviourState};

handle_cast(Request,
            #behaviour_state{worker_state = WorkerState,
                             worker_module = WorkerModule,
                             worker_stats = OldWorkerStats} =
                BehaviourState) ->
    NewWorkerStats = calc_stats(cast, OldWorkerStats),
    case WorkerModule:handle_cast(Request, WorkerState) of
        {noreply, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState};
        {noreply, NewWorkerState, hibernate} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, hibernate};
        {noreply, NewWorkerState, {continue, Continue}} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, {continue, Continue}};
        {noreply, NewWorkerState, Timeout} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, Timeout};
        {stop, WorkerStopReason, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {stop, WorkerStopReason, NewBehaviourState}
    end.

handle_info(Info,
            #behaviour_state{worker_state = WorkerState,
                             worker_module = WorkerModule,
                             worker_stats = OldWorkerStats} =
                BehaviourState) ->
    NewWorkerStats = calc_stats(info, OldWorkerStats),
    case WorkerModule:handle_info(Info, WorkerState) of
        {noreply, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState};
        {noreply, NewWorkerState, hibernate} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, hibernate};
        {noreply, NewWorkerState, {continue, Continue}} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, {continue, Continue}};
        {noreply, NewWorkerState, Timeout} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {noreply, NewBehaviourState, Timeout};
        {stop, WorkerStopReason, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState,
                                                               worker_stats =
                                                                   NewWorkerStats},
            {stop, WorkerStopReason, NewBehaviourState}
    end.

terminate(Reason, #behaviour_state{worker_state = WorkerState,
                                   worker_module = WorkerModule,
                                   pool_name = PoolName}) ->
    ok = WorkerModule:terminate(Reason, WorkerState),

    LeavePoolSignal = #yawp_signal{name = worker_leave_pool,
                                   sender = self(),
                                   receivers = [PoolName]},
    ok = yawp_utils:send_signal(LeavePoolSignal),

    ok.

code_change(OldVsn,
            #behaviour_state{worker_state = WorkerState,
                             worker_module = WorkerModule} = BehaviourState,
            Extra) ->
    case WorkerModule:code_change(OldVsn, WorkerState, Extra) of
        {ok, NewWorkerState} ->
            NewBehaviourState = BehaviourState#behaviour_state{worker_state =
                                                                   NewWorkerState},
            {ok, NewBehaviourState};
        {error, WorkerErrorReason} ->
            {error, WorkerErrorReason}
    end.

format_status(Opt, [WorkerPDict, BehaviourState]) ->
    #behaviour_state{worker_state = WorkerState,
                     worker_module = WorkerModule} = BehaviourState,
    [NewWorkerPDict, NewWorkerState] =
        WorkerModule:format_status(Opt, [WorkerPDict, WorkerState]),
    [NewWorkerPDict,
     BehaviourState#behaviour_state{worker_state = NewWorkerState}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
calc_stats(call, #yawp_worker_stats{calls = OldCalls} = Stats) ->
    Stats#yawp_worker_stats{calls = OldCalls + 1};
calc_stats(cast, #yawp_worker_stats{casts = OldCasts} = Stats) ->
    Stats#yawp_worker_stats{casts = OldCasts + 1};
calc_stats(info, #yawp_worker_stats{infos = OldInfos} = Stats) ->
    Stats#yawp_worker_stats{infos = OldInfos + 1};
calc_stats({offload, call}, #yawp_worker_stats{ol_calls = OldOlCalls} = Stats) ->
    Stats#yawp_worker_stats{ol_calls = OldOlCalls + 1};
calc_stats({offload, cast}, #yawp_worker_stats{ol_casts = OldOlCasts} = Stats) ->
    Stats#yawp_worker_stats{ol_casts = OldOlCasts + 1};
calc_stats({offload, info}, #yawp_worker_stats{ol_infos = OldOlInfos} = Stats) ->
    Stats#yawp_worker_stats{ol_infos = OldOlInfos + 1}.
